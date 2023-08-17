package handrail

import cats.syntax.all._
import cats.parse.{Parser0, Parser, Numbers}

import handrail.model._

class HandlebarsParser(helpersRegistry: HelpersRegistry) {

  private val lookupHelper = helpersRegistry.unsafeHelper("lookup")
  private val renderHelper = helpersRegistry.unsafeHelper("render")
  private val escapeHelper = helpersRegistry.unsafeHelper("escape")
  private val templateHelper = helpersRegistry.unsafeHelper("template")
  private val thisHelper = helpersRegistry.unsafeHelper("this")
  private val missingHelper = helpersRegistry.helper("missing")

  private val thisFunction = thisHelper(List.empty, Map.empty)
  private val notAllowedChars = Set('!', '"', '#', '%', '&', '\'', '(', ')', '*', '+', ',', '.', '/', ';', '<', '=',
    '>', '@', '[', '\\', ']', '^', '`', '{', '|', '}', '~')

  lazy val WhiteSpace: Parser[Unit] = Parser.charIn(' ', '\t', '\n', '\r').void
  lazy val OpenCurlyBrace: Parser[Unit] = Parser.char('{')
  lazy val CloseCurlyBrace: Parser[Unit] = Parser.char('}')
  lazy val OpenParen: Parser[Unit] = Parser.char('(')
  lazy val CloseParen: Parser[Unit] = Parser.char(')')
  lazy val OpenSquareBrace: Parser[Unit] = Parser.char('[')
  lazy val CloseSquareBrace: Parser[Unit] = Parser.char(']')
  lazy val Bang: Parser[Unit] = Parser.char('!')
  lazy val Sharp: Parser[Unit] = Parser.char('#')
  lazy val Slash: Parser[Unit] = Parser.char('/')
  lazy val Tilde: Parser[Unit] = Parser.char('~')
  lazy val Hyphen: Parser[Unit] = Parser.char('-')
  lazy val Equal: Parser[Unit] = Parser.char('=')
  lazy val Quote: Parser[Unit] = Parser.char('"')
  lazy val Stop = WhiteSpace | CloseParen | CloseCurlyBrace | Parser.end
  lazy val This = Parser.string("this")
  lazy val Dot = Parser.char('.')
  lazy val DoubleDot = Dot.rep(2, 2)

  lazy val TwoOpenCurlyBraces: Parser[Unit] = (OpenCurlyBrace *> OpenCurlyBrace)
  lazy val TwoCloseCurlyBraces: Parser[Unit] = (CloseCurlyBrace *> CloseCurlyBrace)
  lazy val TwoHyphen: Parser[Unit] = (Hyphen *> Hyphen)

  lazy val StartExpr: Parser[model.EatSpace] =
    ((TwoOpenCurlyBraces *> Tilde.?).soft <* !Parser.oneOf(List(Sharp, Slash, Bang)))
      .map(_.fold(model.EatSpace.none)(_ => model.EatSpace.left))
  lazy val EndExpr: Parser0[model.EatSpace] =
    (Tilde.? <* TwoCloseCurlyBraces).map(_.fold(model.EatSpace.none)(_ => model.EatSpace.right))

  lazy val StartUnescapedExpr: Parser[model.EatSpace] =
    (TwoOpenCurlyBraces *> Tilde.? <* OpenCurlyBrace).map(_.fold(model.EatSpace.none)(_ => model.EatSpace.left))
  lazy val EndUnescapedExpr: Parser[model.EatSpace] =
    (CloseCurlyBrace *> Tilde.? <* TwoCloseCurlyBraces).map(_.fold(model.EatSpace.none)(_ => model.EatSpace.right))

  lazy val StartUnescapedComment: Parser[model.EatSpace] =
    ((OpenCurlyBrace *> OpenCurlyBrace *> Tilde.?).soft <* Bang)
      .map(_.fold(model.EatSpace.none)(_ => model.EatSpace.left))

  lazy val StartEscapedComment: Parser[model.EatSpace] =
    ((OpenCurlyBrace *> OpenCurlyBrace *> Tilde.?).soft <* Bang <* TwoHyphen)
      .map(_.fold(model.EatSpace.none)(_ => model.EatSpace.left))

  lazy val EndEscapedComment: Parser[model.EatSpace] =
    ((TwoHyphen *> OpenCurlyBrace *> OpenCurlyBrace *> Tilde.?).soft <* Bang)
      .map(_.fold(model.EatSpace.none)(_ => model.EatSpace.left))

  lazy val UnescapedComment: Parser[model.Expression] =
    (StartUnescapedComment ~ (WhiteSpace.? *> Parser.until(WhiteSpace.? *> EndExpr) <* WhiteSpace.?) ~ EndExpr)
      .as(model.Expression.Value.Void)

  lazy val EscapedComment: Parser[model.Expression] =
    (StartEscapedComment ~ (WhiteSpace.? *> Parser.until(
      WhiteSpace.? *> EndEscapedComment
    ) <* WhiteSpace.?) ~ EndExpr).as(model.Expression.Value.Void)

  /*
   * Parsing of expressions
   *
   * {foo} - {lookup this "foo"}
   * {foo.bar} - {lookup (lookup this "foo") "bar"}
   * {loud foo.bar} - {loud (lookup (lookup this "foo") "bar")}
   * {../foo} {lookup (lookup this "..") "foo"}
   * {./foo} - {lookup this "foo"}
   * {[foo.bar]} - {lookup this "foo.bar"}
   * {foo.[0]} - {lookup (lookup this "foo") 0}
   * {foo.[0].bar} - {lookup (lookup (lookup this "foo") 0) "bar"}
   */
  val HelperP: Parser[model.Expression.Function] = {

    val HelperNameP: Parser[String] =
      (Quote *> Parser.until(Quote) <* Quote | Parser.until(Stop))

    lazy val ParameterValueP: Parser[model.Expression] =
      Parser
        .oneOf(
          List(
            ValueLiteralP,
            Parser.defer(HelperP).between(OpenParen *> WhiteSpace.rep0, WhiteSpace.rep0 <* CloseParen),
            Parser.defer(onlyHelpeNameP)
          )
        )
        .withContext("ParameterValueP")

    lazy val PositionalParametersP: Parser0[List[model.Expression]] =
      ParameterValueP.repSep0(WhiteSpace.rep0).withContext("PositionalParametersP")

    lazy val NominalParameterP: Parser[(String, model.Expression)] =
      ((Parser.until(WhiteSpace | Equal) <* Equal
        .surroundedBy(WhiteSpace.rep0)) ~ ParameterValueP).withContext("NominalParameterP")

    lazy val NominalParametersP: Parser0[Map[String, model.Expression]] =
      NominalParameterP
        .repSep0(WhiteSpace.rep)
        .map { xs =>
          xs.toList.toMap
        }
        .withContext("NominalParametersP")

    lazy val onlyHelpeNameP = HelperNameP
      .map { case helperName =>
        helpersRegistry.helper(helperName).map(helper => helper(List.empty, Map.empty)).getOrElse {
          lookupHelper(List(thisFunction, Expression.Value.String(helperName)), Map.empty)
        }
      }
      .withContext("onlyHelpeNameP")

    val withNominalArgs = (HelperNameP ~ (WhiteSpace.rep *> NominalParametersP))
      .map { case (helperName, namedArguments) =>
        val helper = helpersRegistry
          .helper(helperName)
          .orElse(missingHelper)
          .getOrElse(
            throw new IllegalStateException(s"helper '${helperName}' not found and missing helper is not registered")
          )
        helper(List.empty, namedArguments)
      }
      .withContext("withNominalArgs")

    val withPositionalArgs = (HelperNameP ~ (WhiteSpace.rep *> PositionalParametersP))
      .map { case (helperName, positionalArguments) =>
        val helper = helpersRegistry
          .helper(helperName)
          .orElse(missingHelper)
          .getOrElse(
            throw new IllegalStateException(s"helper '${helperName}' not found and missing helper is not registered")
          )
        helper(positionalArguments, Map.empty)
      }
      .withContext("withPositionalArgs")

    val withPositionalAndNominalArgs =
      (HelperNameP ~ (WhiteSpace.rep *> PositionalParametersP) ~ (WhiteSpace.rep *> NominalParametersP))
        .map { case ((helperName, positionalArguments), namedArguments) =>
          val helper = helpersRegistry
            .helper(helperName)
            .orElse(missingHelper)
            .getOrElse(
              throw new IllegalStateException(s"helper '${helperName}' not found and missing helper is not registered")
            )
          helper(positionalArguments, namedArguments)
        }
        .withContext("withPositionalAndNominalArgs")

    Parser.oneOf(
      List(
        withPositionalAndNominalArgs.backtrack,
        withNominalArgs.backtrack,
        withPositionalArgs.backtrack,
        onlyHelpeNameP.backtrack
      )
    )

  }.withContext("HelperP")

  lazy val StringLiteralP: Parser[model.Expression.Value.String] =
    (Quote *> Parser.until(Quote) <* Quote)
      .map { value =>
        model.Expression.Value.String(value)
      }
      .withContext("StringLiteralP")

  lazy val BooleanLiteralP: Parser[model.Expression.Value.Boolean] =
    (Parser.string("true").as(true) | Parser.string("false").as(false))
      .map { value =>
        model.Expression.Value.Boolean(value)
      }
      .withContext("BooleanLiteralP")

  lazy val NumberLiteralP: Parser[model.Expression.Value.Number] =
    (Parser.charWhere(_.isDigit).repAs[String] ~ (Parser.char('.') *> Parser.charWhere(_.isDigit).repAs[String]).?)
      .map { case (a, b) =>
        model.Expression.Value.Number(s"$a.${b.getOrElse("0")}".toDouble)
      }
      .withContext("NumberLiteralP")

  lazy val ValueLiteralP: Parser[model.Expression.Value] =
    (StringLiteralP | NumberLiteralP | BooleanLiteralP).withContext("ValueLiteralP")

  // lazy val RefP: Parser[model.Expression] = {

  //   val segmentP = Parser.until(CloseSquareBrace).between(OpenSquareBrace, CloseSquareBrace) | Parser.until(
  //     Parser.charIn(notAllowedChars)
  //   )

  //   val segmentSeparatorP = Slash | Dot

  //   def segments(parent: model.Expression): Parser[model.Expression] = {

  //     val segmentIdP: Parser[(model.Expression.Function, Parser[Unit])] =
  //       (
  //         Parser
  //           .until(CloseSquareBrace)
  //           .between(OpenSquareBrace, CloseSquareBrace) |
  //           Parser.until(Parser.charIn(notAllowedChars))
  //       ).map { segmentId =>
  //         val exp = lookupHelper(List(parent, model.Expression.Value.String(segmentId)), Map.empty)
  //         val nextSeparator = Slash | Dot
  //         (exp, nextSeparator)
  //       }

  //     val dotOrTwoDots: Parser[(model.Expression.Function, Parser[Unit])] =
  //       Dot.as('.').repUntilAs[String](Slash).map { segmentId =>
  //         val exp = lookupHelper(List(parent, model.Expression.Value.String(segmentId)), Map.empty)
  //         val nextSeparator = Slash
  //         (exp, nextSeparator)
  //       }

  //     (dotOrTwoDots | segmentIdP).flatMap { case (exp, separator) =>
  //       ((separator *> Parser.defer(segments(exp))) | Parser.pure(exp))
  //     }
  //   }

  //   val thisP = This.map { _ =>
  //     thisHelper(List.empty, Map.empty)
  //   }

  //   val jsString = (Quote *> Parser.until(Quote) <* Quote).map { property =>
  //     lookupHelper(List(thisFunction, model.Expression.Value.String(property)), Map.empty)
  //   }

  //   Parser.oneOf(
  //     List(
  //       jsString,
  //       (This *> (segmentSeparatorP *> segments(thisFunction))).backtrack,
  //       thisP,
  //       segments(thisFunction)
  //     )
  //   )
  // }.withContext("RefP")

  lazy val EscapedRefP: Parser[model.Expression.Function] = HelperP
    .between(StartExpr *> WhiteSpace.rep0, WhiteSpace.rep0 *> EndExpr)
    .map(exp => renderHelper(List(escapeHelper(List(exp), Map.empty)), Map.empty))
    .withContext("EscapedRefP")

  lazy val UnescapedRefP: Parser[model.Expression.Function] = HelperP
    .between(StartUnescapedExpr *> WhiteSpace.rep0, WhiteSpace.rep0 *> EndUnescapedExpr)
    .map(exp => renderHelper(List(exp), Map.empty))
    .withContext("UnescapedRefP")

  lazy val TextP: Parser[model.Expression] = Parser
    .until(OpenCurlyBrace *> OpenCurlyBrace | Parser.end)
    .map(text => renderHelper(List(model.Expression.Value.String(text)), Map.empty))
    .withContext("Text")

  lazy val TemplateP: Parser0[model.Expression] =
    (Parser.start *> Parser
      .oneOf(
        List(
          UnescapedComment.backtrack,
          EscapedComment.backtrack,
          UnescapedRefP.backtrack,
          EscapedRefP.backtrack,
          TextP.backtrack
        )
      )
      .rep0
      .map { exps =>
        templateHelper(List(model.Expression.Value.Array(exps)), Map.empty)
      } <* Parser.end).withContext("Template")

  // lazy val BlockName: Parser[String] = P(CharsWhile(c => c != '}' && c != ' ').!)
  // val BlockArguments: Parser[Seq[String]] = P(BlockName.rep(sep = (Space ~/ Pass).rep(min = 1)))

  // lazy val OpenBlock: Parser[(String, Seq[String])] = P(
  //   StartOpenBlock ~/ BlockName ~ (Space.rep(min = 0) ~ BlockArguments).?.map(_.getOrElse(Seq.empty)) ~ EndExpr
  // )
  // def closeBlock(blockName: String): Parser[Unit] =
  //   P(StartCloseBlock ~/ BlockName.filter(_ == blockName) ~ EndExpr).map(_ => ())

  // lazy val Block: Parser[model.Block] = OpenBlock.flatMap { case (name, args) =>
  //   P(Asts ~ closeBlock(name)).map(xs => model.Block(name, args.toVector, xs.toList))
  // }

  // lazy val Ast: Parser[model.Ast] = P(Ref | UnescapedRef | Text | Block)

  // lazy val Asts: Parser[Seq[model.Ast]] = model.rep(sep = P(Comment | Pass))

  // lazy val Handlebars: Parser[Seq[model.Ast]] = Start ~ Asts ~ End

}
