package handrail

import cats.syntax.all._
import cats.parse.{Parser0, Parser, Numbers}

object HandlebarsParser {

  private val thisFunction = ast.Expression.Function("this")
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

  lazy val Identifier: Parser[String] =
    (Parser.charWhere(_.isLetter) ~ Parser.until(
      !(Parser.charWhere(_.isLetterOrDigit) | Parser.char('_') | Parser.char('-'))
    ))
      .map { case (ch, string) =>
        ch :: string.toList
      }
      .map(_.mkString)

  lazy val Text: Parser[ast.Text] = Parser.until(OpenCurlyBrace).map(ast.Text)

  lazy val TwoOpenCurlyBraces: Parser[Unit] = (OpenCurlyBrace *> OpenCurlyBrace)
  lazy val TwoCloseCurlyBraces: Parser[Unit] = (CloseCurlyBrace *> CloseCurlyBrace)
  lazy val TwoHyphen: Parser[Unit] = (Hyphen *> Hyphen)

  lazy val StartExpr: Parser[ast.EatSpace] =
    ((TwoOpenCurlyBraces *> Tilde.?).soft <* !Parser.oneOf(List(Sharp, Slash, Bang)))
      .map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))
  lazy val EndExpr: Parser0[ast.EatSpace] =
    (Tilde.? <* TwoCloseCurlyBraces).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.right))

  lazy val StartUnescapedExpr: Parser[ast.EatSpace] =
    (TwoOpenCurlyBraces *> Tilde.? <* OpenCurlyBrace).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))
  lazy val EndUnescapedExpr: Parser[ast.EatSpace] =
    (CloseCurlyBrace *> Tilde.? <* TwoCloseCurlyBraces).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.right))

  lazy val StartUnescapedComment: Parser[ast.EatSpace] =
    ((OpenCurlyBrace *> OpenCurlyBrace *> Tilde.?).soft <* Bang).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))

  lazy val StartEscapedComment: Parser[ast.EatSpace] =
    ((OpenCurlyBrace *> OpenCurlyBrace *> Tilde.?).soft <* Bang <* TwoHyphen)
      .map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))

  lazy val EndEscapedComment: Parser[ast.EatSpace] =
    ((TwoHyphen *> OpenCurlyBrace *> OpenCurlyBrace *> Tilde.?).soft <* Bang)
      .map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))

  lazy val UnescapedComment: Parser[ast.Comment] =
    (StartUnescapedComment ~ (WhiteSpace.? *> Parser.until(WhiteSpace.? *> EndExpr) <* WhiteSpace.?) ~ EndExpr).map {
      case ((eatSpaceLeft, commentContent), eatSpaceRight) =>
        ast.Comment(
          commentContent,
          escaped = false,
          eatSpace = eatSpaceLeft.combine(eatSpaceRight)
        )
    }

  lazy val EscapedComment: Parser[ast.Comment] =
    (StartEscapedComment ~ (WhiteSpace.? *> Parser.until(WhiteSpace.? *> EndEscapedComment) <* WhiteSpace.?) ~ EndExpr)
      .map { case ((eatSpaceLeft, commentContent), eatSpaceRight) =>
        ast.Comment(
          commentContent,
          escaped = true,
          eatSpace = eatSpaceLeft.combine(eatSpaceRight)
        )
      }

  lazy val StartOpenBlock: Parser[ast.EatSpace] =
    ((TwoOpenCurlyBraces *> Tilde.?).soft <* Sharp).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))
  lazy val StartCloseBlock: Parser[ast.EatSpace] =
    (TwoCloseCurlyBraces *> Tilde.? <* Slash).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.right))

  val HelperP: Parser[ast.Expression.Function] = {

    val HelperNameP: Parser[String] = Quote *> Parser.until(Quote) <* Quote | Parser.until(Stop)

    lazy val ParameterValueP: Parser[ast.Expression] =
      Parser.oneOf(
        List(
          ValueLiteralP,
          Parser.defer(HelperP).between(OpenParen *> WhiteSpace.rep0, WhiteSpace.rep0 <* CloseParen)
        )
      )

    lazy val PositionalParametersP: Parser0[List[ast.Expression]] =
      ParameterValueP.repSep0(WhiteSpace.rep0).withContext("PositionalParametersP")

    val NominalParameterP: Parser[(String, ast.Expression)] =
      (Parser.until(WhiteSpace | Equal) <* Equal
        .surroundedBy(WhiteSpace.rep0)) ~ ParameterValueP

    val NominalParametersP: Parser0[Map[String, ast.Expression]] =
      NominalParameterP
        .repSep0(WhiteSpace.rep)
        .map { xs =>
          xs.toList.toMap
        }
        .withContext("NominalParametersP")

    val onlyHelpeNamerP = HelperNameP.map { case helperName =>
      ast.Expression.Function(helperName)
    }

    val withNominalArgs = (HelperNameP ~ (WhiteSpace.rep *> NominalParametersP)).map { case (helperName, nominalArgs) =>
      ast.Expression.Function(helperName, namedArguments = nominalArgs)
    }

    val withPositionalArgs = (HelperNameP ~ (WhiteSpace.rep *> PositionalParametersP)).map {
      case (helperName, positionalParameters) =>
        ast.Expression.Function(helperName, positionalArguments = positionalParameters)
    }

    val withPositionalAndNominalArgs =
      (HelperNameP ~ (WhiteSpace.rep *> PositionalParametersP) ~ (WhiteSpace.rep *> NominalParametersP)).map {
        case ((helperName, positionalParameters), nominalParameters) =>
          ast.Expression.Function(
            helperName,
            positionalArguments = positionalParameters,
            namedArguments = nominalParameters
          )
      }

    Parser.oneOf(
      List(
        withPositionalAndNominalArgs.backtrack,
        withNominalArgs.backtrack,
        withPositionalArgs.backtrack,
        onlyHelpeNamerP.backtrack
      )
    )

  }

  lazy val StringLiteralP: Parser[ast.Expression.Value.String] =
    (Quote *> Parser.until(Quote) <* Quote).map { value =>
      ast.Expression.Value.String(value)
    }

  lazy val BooleanLiteralP: Parser[ast.Expression.Value.Boolean] =
    (Parser.string("true").as(true) | Parser.string("false").as(false)).map { value =>
      ast.Expression.Value.Boolean(value)
    }

  lazy val NumberLiteralP: Parser[ast.Expression.Value.Number] =
    (Parser.charWhere(_.isDigit).repAs[String] ~ (Parser.char('.') *> Parser.charWhere(_.isDigit).repAs[String]).?)
      .map { case (a, b) =>
        ast.Expression.Value.Number(s"$a.${b.getOrElse("0")}".toDouble)
      }

  lazy val ValueLiteralP: Parser[ast.Expression.Value] = StringLiteralP | NumberLiteralP | BooleanLiteralP

  lazy val EscapedRefP: Parser[ast.Expression.Function] = HelperP
    .between(StartExpr *> WhiteSpace.rep0, WhiteSpace.rep0 *> EndExpr)
    .map(exp => ast.Expression.Function("render", List(ast.Expression.Function("escape", List(exp)))))

  lazy val UnescapedRefP: Parser[ast.Expression.Function] = HelperP
    .between(StartUnescapedExpr *> WhiteSpace.rep0, WhiteSpace.rep0 *> EndUnescapedExpr)
    .map(exp => ast.Expression.Function("render", List(exp)))

  lazy val RefP: Parser[ast.Expression] = {

    val segmentP = Parser.until(CloseSquareBrace).between(OpenSquareBrace, CloseSquareBrace) | Parser.until(
      Parser.charIn(notAllowedChars)
    )

    val segmentSeparatorP = Slash | Dot

    def segments(parent: ast.Expression): Parser[ast.Expression] = {

      val segmentIdP: Parser[(ast.Expression.Function, Parser[Unit])] =
        (
          Parser
            .until(CloseSquareBrace)
            .between(OpenSquareBrace, CloseSquareBrace) |
            Parser.until(Parser.charIn(notAllowedChars))
        ).map { segmentId =>
          val exp = ast.Expression.Function("lookup", List(parent, ast.Expression.Value.String(segmentId)))
          val nextSeparator = Slash | Dot
          (exp, nextSeparator)
        }

      val dotOrTwoDots: Parser[(ast.Expression.Function, Parser[Unit])] =
        Dot.as('.').repUntilAs[String](Slash).map { segmentId =>
          val exp = ast.Expression.Function("lookup", List(parent, ast.Expression.Value.String(segmentId)))
          val nextSeparator = Slash
          (exp, nextSeparator)
        }

      (dotOrTwoDots | segmentIdP).flatMap { case (exp, separator) =>
        ((separator *> Parser.defer(segments(exp))) | Parser.pure(exp))
      }
    }

    val thisP = This.as(thisFunction)
    val jsString = (Quote *> Parser.until(Quote) <* Quote).map { property =>
      ast.Expression.Function("lookup", List(thisFunction, ast.Expression.Value.String(property)))
    }

    Parser.oneOf(
      List(
        jsString,
        (This *> (segmentSeparatorP *> segments(thisFunction))).backtrack,
        thisP,
        segments(thisFunction)
      )
    )
  }
  // val segment = Parser.charWhere(c => !notAllowedChars.contains(c)).repAs[String]

  // lazy val SegmentP: Parser[ast.Expression.Function] = Parser.recursive[ast.Expression.Function] { recurse =>
  //   (Dot *> segment) ~ recurse
  // }

  // val thisP = This.as(thisFunction)
  // val dotP = Dot.as(thisFunction)

  // (StartExpr ~ (WhiteSpace.? *> HelperP <* WhiteSpace.?) ~ EndExpr).map { case ((eatSpaceLeft, f), eatSpaceRight) =>
  //   f
  // }
  // lazy val BlockName: Parser[String] = P(CharsWhile(c => c != '}' && c != ' ').!)
  // val BlockArguments: Parser[Seq[String]] = P(BlockName.rep(sep = (Space ~/ Pass).rep(min = 1)))

  // lazy val OpenBlock: Parser[(String, Seq[String])] = P(
  //   StartOpenBlock ~/ BlockName ~ (Space.rep(min = 0) ~ BlockArguments).?.map(_.getOrElse(Seq.empty)) ~ EndExpr
  // )
  // def closeBlock(blockName: String): Parser[Unit] =
  //   P(StartCloseBlock ~/ BlockName.filter(_ == blockName) ~ EndExpr).map(_ => ())

  // lazy val Block: Parser[ast.Block] = OpenBlock.flatMap { case (name, args) =>
  //   P(Asts ~ closeBlock(name)).map(xs => ast.Block(name, args.toVector, xs.toList))
  // }

  // lazy val Ast: Parser[ast.Ast] = P(Ref | UnescapedRef | Text | Block)

  // lazy val Asts: Parser[Seq[ast.Ast]] = Ast.rep(sep = P(Comment | Pass))

  // lazy val Handlebars: Parser[Seq[ast.Ast]] = Start ~ Asts ~ End

}
