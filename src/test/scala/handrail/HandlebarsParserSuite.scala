package handrail

import cats.syntax.all._
import cats.parse._

class HandlebarsParserSuite extends munit.FunSuite {

  import HandlebarsParser._

  test("Identifier should parse an identifier before space") {
    val source = "foo   "
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("   ", "foo")))
  }

  test("Identifier should parse an identifier before \t") {
    val source = "foo\t"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("\t", "foo")))
  }

  test("Identifier should parse an identifier before \n") {
    val source = "foo\n"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("\n", "foo")))
  }

  test("Identifier should parse an identifier before \r") {
    val source = "foo\r"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("\r", "foo")))
  }

  test("Identifier should parse an identifier before }") {
    val source = "foo}"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("}", "foo")))
  }

  test("Identifier should parse an identifier before end") {
    val source = "foo"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("", "foo")))
  }

  test("Identifier should parse an identifier with numbers") {
    val source = "f123"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("", "f123")))
  }

  test("Identifier should parse an identifier with underscore") {
    val source = "f_123"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("", "f_123")))
  }

  test("Identifier should parse an identifier with dash") {
    val source = "f-123"
    val result = Identifier.parse(source)

    assertEquals(clue(result), Right(("", "f-123")))
  }

  test("Identifier should not parse an identifier starting with numbers") {
    val source = "123"
    val result = Identifier.parse(source)

    assert(clue(result).isLeft)
  }

  test("Space should parse a blank string") {
    val source = "   \t  "
    val result = WhiteSpace.parse(source)

    assert(clue(result).isRight)
  }

  test("Text should parse a random text") {
    val source = "The quick brown fox jumps over the lazy dog"
    val result = Text.parseAll(source)

    assertEquals(clue(result), ast.Text(source).asRight[Parser.Error])
  }

  test("Text should parse a random multilinetext") {
    val source = """
    The quick brown fox 
    jumps over 
    the lazy dog
    """
    val result = Text.parseAll(source)

    assertEquals(clue(result), ast.Text(source).asRight[Parser.Error])
  }

  test("EscapedRef should parse a Ref") {
    val source = """{{ foo }}"""
    val result = EscapedRef.parseAll(source)

    assertEquals(clue(result), ast.Ref("foo").asRight[Parser.Error])
  }

  test("EscapedRef should parse a Ref with EatSpace.Left") {
    val source = """{{~ foo }}"""
    val result = EscapedRef.parse(source)

    assertEquals(clue(result), ("", ast.Ref("foo", eatSpace = ast.EatSpace.Left)).asRight[Parser.Error])
  }

  test("EscapedRef should parse a Ref with EatSpace.Right") {
    val source = """{{ foo ~}}"""
    val result = EscapedRef.parse(source)

    assertEquals(clue(result), ("", ast.Ref("foo", eatSpace = ast.EatSpace.Right)).asRight[Parser.Error])
  }

  test("EscapedRef should parse a Ref with EatSpace.Both") {
    val source = """{{~ foo ~}}"""
    val result = EscapedRef.parse(source)

    assertEquals(clue(result), ("", ast.Ref("foo", eatSpace = ast.EatSpace.Both)).asRight[Parser.Error])
  }

  test("UnescapeRef should parse a Ref") {
    val source = """{{{ foo }}}"""
    val result = UnescapedRef.parseAll(source)

    assertEquals(clue(result), ast.Ref("foo", escaped = false).asRight[Parser.Error])
  }

  test("UnescapeRef should parse a Ref with EatSpace.Both") {
    val source = """{{~{ foo }~}}"""
    val result = UnescapedRef.parseAll(source)

    assertEquals(clue(result), ast.Ref("foo", escaped = false, eatSpace = ast.EatSpace.Both).asRight[Parser.Error])
  }

  // test("Comment should parse an unescaped comment") {
  //   val source = "{{! foo bar }}"
  //   val result = Comment.parseAll(source)

  //   assertEquals(clue(result), ast.Comment("foo bar").asRight[Parser.Error])
  // }

  // test("Comment should parse an unescaped comment with EatSpace.Left") {
  //   val source = "{{~! foo bar }}"
  //   val result = Comment.parseAll(source)

  //   assertEquals(clue(result), ast.Comment("foo bar", eatSpace = ast.EatSpace.Left).asRight[Parser.Error])
  // }

  // test("Comment should parse an unescaped comment with EatSpace.Right") {
  //   val source = "{{! foo bar ~}}"
  //   val result = Comment.parseAll(source)

  //   assertEquals(clue(result), ast.Comment("foo bar", eatSpace = ast.EatSpace.Right).asRight[Parser.Error])
  // }

  // test("Comment should parse an unescaped comment with EatSpace.Both") {
  //   val source = "{{~! foo bar ~}}"
  //   val result = Comment.parseAll(source)

  //   assertEquals(clue(result), ast.Comment("foo bar", eatSpace = ast.EatSpace.Both).asRight[Parser.Error])
  // }
}

// class ParserSpec extends WordSpec with Matchers {

//   import ast._

//   "Ref" should {
//     "parse an Ref" in {
//       val source = "{{ title }}"
//       val result = Parser.Ref.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe Ref("title")
//     }

//     "parse a simple text and Ref" in {
//       val source = "<p>{{ title }}</p>"
//       val result = Parser.Asts.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe List(Text("<p>"), Ref("title"), Text("</p>"))
//     }

//     "parse a simple text and Ref across multiple lines" in {
//       val source = "<p>{{ \n\ntitle\n }}</p>"
//       val result = Parser.Asts.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe List(Text("<p>"), Ref("title"), Text("</p>"))
//     }
//   }

//   "UnescapedRef" should {
//     "parse an UnescapedRef" in {
//       val source = "{{{ title }}}"
//       val result = Parser.UnescapedRef.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe Ref("title", escaped = false)
//     }

//     "parse a simple text and UnescapedRef" in {
//       val source = "<p>{{{ title }}}</p>"
//       val result = Parser.Asts.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe List(Text("<p>"), Ref("title", escaped = false), Text("</p>"))
//     }
//   }

//   "OpenBlock" should {
//     "parse an open block" in {
//       val source = "{{#foo}}"
//       val result = Parser.OpenBlock.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe Tuple2("foo", Vector.empty)
//     }

//     "parse an open block with an argument" in {
//       val source = "{{#foo bar}}"
//       val result = Parser.OpenBlock.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe Tuple2("foo", Vector("bar"))
//     }
//   }

//   "Comment" should {
//     "parse a comment without --" in {
//       val source = "{{! foo bar }}"
//       val result = Parser.Comment.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(_, index) = result
//       index shouldBe source.length
//     }
//   }

//   "Block" should {
//     "parse a simple block" in {
//       val source = "{{#each people}}\n\t<p>{{firstName}} {{lastName}}</p>\n{{/each}}"
//       val result = Parser.Block.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe Block("each", Vector("people"), Vector(Text("\n\t<p>"), Ref("firstName"), Text(" "), Ref("lastName"), Text("</p>\n")))
//     }

//     "parse a nested block" in {
//       val source = "{{#each people}}\n\t<p>{{firstName}} {{lastName}}</p>\n\t<ul>\n\t{{#each addresses}}\n\t\t<li>{{street}} {{postCode}}</li>\n\t{{/each}}\n\t</ul>\n{{/each}}"
//       val result = Parser.Block.parse(source)
//       result shouldBe a[Parsed.Success[_]]

//       val Parsed.Success(value, index) = result
//       index shouldBe source.length
//       value shouldBe Block("each", Vector("people"), Vector(
//         Text("\n\t<p>"),
//         Ref("firstName"),
//         Text(" "),
//         Ref("lastName"),
//         Text("</p>\n\t<ul>\n\t"),
//         Block("each", Vector("addresses"), Vector(
//           Text("\n\t\t<li>"), Ref("street"), Text(" "), Ref("postCode"), Text("</li>\n\t"),
//         )),
//         Text("\n\t</ul>\n")
//       ))
//     }
//   }

// }
