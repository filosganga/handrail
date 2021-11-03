package handrail

import cats.syntax.all._
import cats.parse.{Parser0, Parser, Numbers}

object HandlebarsParser {

  lazy val WhiteSpace: Parser[Unit] = Parser.charIn(' ', '\t', '\n', '\r').void
  lazy val OpenCurlyBrace: Parser[Unit] = Parser.char('{')
  lazy val CloseCurlyBrace: Parser[Unit] = Parser.char('}')
  lazy val Bang: Parser[Unit] = Parser.char('!')
  lazy val Sharp: Parser[Unit] = Parser.char('#')
  lazy val Slash: Parser[Unit] = Parser.char('/')
  lazy val Tilde: Parser[Unit] = Parser.char('~')
  lazy val Hyphen: Parser[Unit] = Parser.char('-')

  lazy val Identifier: Parser[String] =
    (Parser.charWhere(_.isLetter) ~ Parser.until(
      !(Parser.charWhere(_.isLetterOrDigit) | Parser.char('_') | Parser.char('-'))
    ))
      .map { case (ch, string) =>
        ch :: string.toList
      }
      .map(_.mkString)

  lazy val Text: Parser[ast.Text] = Parser.until(OpenCurlyBrace).map(ast.Text)

  /* {{ <- legal
   * {{# <- legal
   * {{! <- legal
   * {{!-- <- legal
   * {{~ <- legal
   * {{{ <- legal
   * {{~{ <- legal
   * {{~# <- legal
   * {{~! <- legal
   * {{~!-- <- legal
   */

  lazy val TwoOpenCurlyBraces: Parser[Unit] = (OpenCurlyBrace *> OpenCurlyBrace)
  lazy val TwoCloseCurlyBraces: Parser[Unit] = (CloseCurlyBrace *> CloseCurlyBrace)
  lazy val TwoHyphen: Parser[Unit] = (Hyphen *> Hyphen)

  lazy val StartExpr: Parser[ast.EatSpace] =
    ((TwoOpenCurlyBraces *> Tilde.?).soft <* !Parser.oneOf(List(Sharp, Slash, Bang)))
      .map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))
  lazy val EndExpr: Parser0[ast.EatSpace] =
    (Tilde.? <* TwoCloseCurlyBraces).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.right))

  lazy val EscapedRef: Parser[ast.Ref] = (StartExpr ~ (WhiteSpace.? *> Identifier <* WhiteSpace.?) ~ EndExpr).map {
    case ((eatSpaceLeft, name), eatSpaceRight) =>
      ast.Ref(name, escaped = true, eatSpace = eatSpaceLeft.combine(eatSpaceRight))
  }

  lazy val StartUnescapedExpr: Parser[ast.EatSpace] =
    (TwoOpenCurlyBraces *> Tilde.? <* OpenCurlyBrace).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.left))
  lazy val EndUnescapedExpr: Parser[ast.EatSpace] =
    (CloseCurlyBrace *> Tilde.? <* TwoCloseCurlyBraces).map(_.fold(ast.EatSpace.none)(_ => ast.EatSpace.right))

  lazy val UnescapedRef: Parser[ast.Ref] =
    (StartUnescapedExpr ~ (WhiteSpace.? *> Identifier <* WhiteSpace.?) ~ EndUnescapedExpr).map {
      case ((eatSpaceLeft, name), eatSpaceRight) =>
        ast.Ref(name, escaped = false, eatSpace = eatSpaceLeft.combine(eatSpaceRight))
    }

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
