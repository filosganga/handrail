package handrail

import cats.syntax.all._
import cats.parse._

class HandlebarsParserSuite extends munit.FunSuite {

  import HandlebarsParser._

  test("Identifier should parse an identifier before space") {
    val source = "foo   "
    val result = Identifier.parse(source)

    assertEquals(result, Right(("   ", "foo")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier before \t") {
    val source = "foo\t"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("\t", "foo")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier before \n") {
    val source = "foo\n"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("\n", "foo")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier before \r") {
    val source = "foo\r"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("\r", "foo")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier before }") {
    val source = "foo}"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("}", "foo")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier before end") {
    val source = "foo"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("", "foo")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier with numbers") {
    val source = "f123"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("", "f123")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier with underscore") {
    val source = "f_123"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("", "f_123")), s"Cannot parse \"$source\"")
  }

  test("Identifier should parse an identifier with dash") {
    val source = "f-123"
    val result = Identifier.parse(source)

    assertEquals(result, Right(("", "f-123")), s"Cannot parse \"$source\"")
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

    assertEquals(result, ast.Text(source).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("Text should parse a random multilinetext") {
    val source = """
    The quick brown fox 
    jumps over 
    the lazy dog
    """
    val result = Text.parseAll(source)

    assertEquals(result, ast.Text(source).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("StringLiteralP should parse a string") {
    val source = """"foo bar""""
    val result = StringLiteralP.parse(source)

    assertEquals(
      result,
      ("", ast.Expression.Value.String("foo bar")).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("BooleanLiteralP should parse true") {
    val source = """true"""
    val result = BooleanLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Boolean(true)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("BooleanLiteralP should parse false") {
    val source = """false"""
    val result = BooleanLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Boolean(false)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("NumberLiteralP should parse an int") {
    val source = """5"""
    val result = NumberLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Number(5)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("NumberLiteralP should parse a decimal") {
    val source = """5.2"""
    val result = NumberLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Number(5.2)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("ValueLiteralP should parse a string") {
    val source = """"foo bar""""
    val result = ValueLiteralP.parse(source)

    assertEquals(
      result,
      ("", ast.Expression.Value.String("foo bar")).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("ValueLiteralP should parse true") {
    val source = """true"""
    val result = ValueLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Boolean(true)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("ValueLiteralP should parse false") {
    val source = """false"""
    val result = ValueLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Boolean(false)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("ValueLiteralP should parse an int") {
    val source = """5"""
    val result = ValueLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Number(5)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  test("ValueLiteralP should parse a decimal") {
    val source = """5.2"""
    val result = ValueLiteralP.parse(source)

    assertEquals(result, ("", ast.Expression.Value.Number(5.2)).asRight[Parser.Error], s"Cannot parse \"$source\"")
  }

  // test("NominalParameterP should parse an assignement with spaces") {
  //   val source = """foo = "bar""""
  //   val result = NominalParameterP.parse(source)

  //   assertEquals(
  //     clue(result),
  //     ("", "foo" -> ast.Expression.Value.String("bar")).asRight[Parser.Error]
  //   )
  // }

  // test("NominalParameterP should parse an assignement without spaces") {
  //   val source = """foo=5"""
  //   val result = NominalParameterP.parse(source)

  //   assertEquals(
  //     clue(result),
  //     ("", "foo" -> ast.Expression.Value.Number(5)).asRight[Parser.Error]
  //   )
  // }

  // test("NominalParametersP should parse a list of assignements") {
  //   val source = """foo="bar" bar=true"""
  //   val result = NominalParametersP.parse(source)

  //   assertEquals(
  //     clue(result),
  //     ("", Map("foo" -> ast.Expression.Value.String("bar"), "bar" -> ast.Expression.Value.Boolean(true)))
  //       .asRight[Parser.Error]
  //   )
  // }

  // test("NominalParametersP should parse a list of assignements with spaces") {
  //   val source = """foo = "bar"   bar =   true"""
  //   val result = NominalParametersP.parse(source)

  //   assertEquals(
  //     clue(result),
  //     ("", Map("foo" -> ast.Expression.Value.String("bar"), "bar" -> ast.Expression.Value.Boolean(true)))
  //       .asRight[Parser.Error]
  //   )
  // }

  // test("PositionalParametersP should parse a list of parameter value") {
  //   val source = """"foo"   5 false"""
  //   val result = PositionalParametersP.parse(source)

  //   assertEquals(
  //     clue(result),
  //     (
  //       "",
  //       List(
  //         ast.Expression.Value.String("foo"),
  //         ast.Expression.Value.Number(5),
  //         ast.Expression.Value.Boolean(false)
  //       )
  //     ).asRight[Parser.Error]
  //   )
  // }

  // test("PositionalParametersP should parse an empty list of parameter value") {
  //   val source = """ """
  //   val result = PositionalParametersP.parse(source)

  //   assertEquals(
  //     clue(result),
  //     (" ", List.empty[ast.Expression]).asRight[Parser.Error]
  //   )
  // }

  test("HelperP should parse an helper without any parameters") {
    val source = """helper"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List.empty,
      Map.empty
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse a quoted helper without any parameters") {
    val source = """"hel per""""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "hel per",
      List.empty,
      Map.empty
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with positinal args") {
    val source = """helper 5"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Value.Number(5)
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with positinal and nominal args") {
    val source = """helper 5 foo=5"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Value.Number(5)
      ),
      Map(
        "foo" -> ast.Expression.Value.Number(5)
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with only nominal args") {
    val source = """helper foo=5"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List.empty,
      Map("foo" -> ast.Expression.Value.Number(5))
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with spaced nominal args") {
    val source = """helper foo =   5"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List.empty,
      Map("foo" -> ast.Expression.Value.Number(5))
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with spaced positional and nominal args") {
    val source = """helper 5   false 8 foo =  true bar =  "baz""""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Value.Number(5),
        ast.Expression.Value.Boolean(false),
        ast.Expression.Value.Number(8)
      ),
      Map(
        "foo" -> ast.Expression.Value.Boolean(true),
        "bar" -> ast.Expression.Value.String("baz")
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse a quotee helper with spaced positional and nominal args") {
    val source = """"hel per" 5   false 8 foo =  true bar =  "baz""""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "hel per",
      List(
        ast.Expression.Value.Number(5),
        ast.Expression.Value.Boolean(false),
        ast.Expression.Value.Number(8)
      ),
      Map(
        "foo" -> ast.Expression.Value.Boolean(true),
        "bar" -> ast.Expression.Value.String("baz")
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper within {{ and }}") {
    val source = """{{helper}}"""
    val result = EscapedRefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "escape",
          List(
            ast.Expression.Function(
              "helper",
              List.empty,
              Map.empty
            )
          )
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper within {{ and }} with spaces") {
    val source = """{{  helper }}"""
    val result = EscapedRefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "escape",
          List(
            ast.Expression.Function(
              "helper",
              List.empty,
              Map.empty
            )
          )
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper within {{{ and }}}") {
    val source = """{{{helper}}}"""
    val result = UnescapedRefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "helper",
          List.empty,
          Map.empty
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with 1 positional argument") {
    val source = """helper 5"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(ast.Expression.Value.Number(5)),
      Map.empty
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse a simple helper") {
    val source = """helper "foo" 5.6 argName1=true argName2=false"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Value.String("foo"),
        ast.Expression.Value.Number(5.6)
      ),
      Map(
        "argName1" -> ast.Expression.Value.Boolean(true),
        "argName2" -> ast.Expression.Value.Boolean(false)
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with 1 nested helper") {
    val source = """helper (helper 5)"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Value.Number(5)
          )
        )
      ),
      Map.empty
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with 1 quoted nested helper") {
    val source = """helper ("hel per" 5)"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Function(
          "hel per",
          List(
            ast.Expression.Value.Number(5)
          )
        )
      ),
      Map.empty
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with 2 nested helper") {
    val source = """helper (helper (helper 5))"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Function(
              "helper",
              List(
                ast.Expression.Value.Number(5)
              )
            )
          )
        )
      ),
      Map.empty
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with 2 quoted nested helper") {
    val source = """helper ("hel per" ("hel per" 5))"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Function(
          "hel per",
          List(
            ast.Expression.Function(
              "hel per",
              List(
                ast.Expression.Value.Number(5)
              )
            )
          )
        )
      ),
      Map.empty
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with 1 nested helper as nominal argument") {
    val source = """helper 6 one=(helper 5)"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Value.Number(6)
      ),
      Map(
        "one" -> ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Value.Number(5)
          ),
          Map.empty
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with 1 quoted nested helper as nominal argument") {
    val source = """helper 6 one=("hel per" 5)"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Value.Number(6)
      ),
      Map(
        "one" -> ast.Expression.Function(
          "hel per",
          List(
            ast.Expression.Value.Number(5)
          ),
          Map.empty
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with many nested quoted helper as nominal argument") {
    val source = """helper 6 one=("hel per" two=("hel per" 5))"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Value.Number(6)
      ),
      Map(
        "one" -> ast.Expression.Function(
          "hel per",
          namedArguments = Map(
            "two" -> ast.Expression.Function(
              "hel per",
              positionalArguments = List(
                ast.Expression.Value.Number(5)
              )
            )
          )
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with many nested helpers ") {
    val source = """helper (helper (helper 5)) one=(helper 5)"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Function(
              "helper",
              List(
                ast.Expression.Value.Number(5)
              )
            )
          )
        )
      ),
      Map(
        "one" -> ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Value.Number(5)
          ),
          Map.empty
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("HelperP should parse an helper with spaced many nested helpers") {
    val source = """helper (helper ( helper 5 ) ) one= ( helper 5 )"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Function(
              "helper",
              List(
                ast.Expression.Value.Number(5)
              )
            )
          )
        )
      ),
      Map(
        "one" -> ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Value.Number(5)
          ),
          Map.empty
        )
      )
    )
    assertEquals(
      clue(result),
      ("", expectedResult).asRight[Parser.Error]
    )
  }

  test("HelperP should parse an helper with many nested helpers with args") {
    val source = """helper (helper (helper 5 one =  1)) one=(helper 5  two = 2)"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "helper",
      List(
        ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Function(
              "helper",
              List(
                ast.Expression.Value.Number(5)
              ),
              Map(
                "one" -> ast.Expression.Value.Number(1)
              )
            )
          )
        )
      ),
      Map(
        "one" -> ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Value.Number(5)
          ),
          Map(
            "two" -> ast.Expression.Value.Number(2)
          )
        )
      )
    )
    assertEquals(
      clue(result),
      ("", expectedResult).asRight[Parser.Error]
    )
  }

  test("HelperP should parse an helper with many nested quoted helpers with args") {
    val source = """"hel per" (helper ("hel per" 5 one=1) ) one=("hel per" 5 two=2)"""
    val result = HelperP.parse(source)

    val expectedResult = ast.Expression.Function(
      "hel per",
      List(
        ast.Expression.Function(
          "helper",
          List(
            ast.Expression.Function(
              "hel per",
              List(
                ast.Expression.Value.Number(5)
              ),
              Map(
                "one" -> ast.Expression.Value.Number(1)
              )
            )
          )
        )
      ),
      Map(
        "one" -> ast.Expression.Function(
          "hel per",
          List(
            ast.Expression.Value.Number(5)
          ),
          Map(
            "two" -> ast.Expression.Value.Number(2)
          )
        )
      )
    )
    assertEquals(
      result,
      ("", expectedResult).asRight[Parser.Error],
      s"Cannot parse \"$source\""
    )
  }

  test("RefP should parse this") {
    val source = """this"""
    val result = RefP.parse(source)

    val expectedResult = ast.Expression.Function("this")

    assertEquals(result, ("", expectedResult).asRight[Parser.Error], s"Cannot parse '$source'")
  }

  test("RefP should parse this with multiple segments") {
    val source = """this.foo.bar"""
    val result = RefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "lookup",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            ast.Expression.Function("this"),
            ast.Expression.Value.String("foo")
          )
        ),
        ast.Expression.Value.String("bar")
      )
    )

    assertEquals(clue(result), ("", expectedResult).asRight[Parser.Error], s"Cannot parse '$source'")
  }

  test("RefP should parse multiple segments") {
    val source = """foo.bar"""
    val result = RefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "lookup",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            ast.Expression.Function("this"),
            ast.Expression.Value.String("foo")
          )
        ),
        ast.Expression.Value.String("bar")
      )
    )

    assertEquals(clue(result), ("", expectedResult).asRight[Parser.Error], s"Cannot parse '$source'")
  }

  test("RefP should parse multiple segments with square brackets") {
    val source = clue("""foo.[bar baz]""")
    val result = RefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "lookup",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            ast.Expression.Function("this"),
            ast.Expression.Value.String("foo")
          )
        ),
        ast.Expression.Value.String("bar baz")
      )
    )

    assertEquals(result, ("", expectedResult).asRight[Parser.Error], s"Cannot parse '$source'")
  }

  test("RefP should parse multiple segments with square brackets separated by /") {
    val source = clue("""foo/[bar baz]""")
    val result = RefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "lookup",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            ast.Expression.Function("this"),
            ast.Expression.Value.String("foo")
          )
        ),
        ast.Expression.Value.String("bar baz")
      )
    )

    assertEquals(result, ("", expectedResult).asRight[Parser.Error], s"Cannot parse '$source'")
  }

  test("RefP should parse ./foo as this.foo") {
    val source = """./foo"""
    val result = RefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "lookup",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            ast.Expression.Function("this"),
            ast.Expression.Value.String(".")
          )
        ),
        ast.Expression.Value.String("foo")
      )
    )

    assertEquals(result, ("", expectedResult).asRight[Parser.Error], s"Cannot parse '$source'")
  }

  test("RefP should parse ../foo as this/../foo") {
    val source = """../foo"""
    val result = RefP.parse(source)

    val expectedResult = ast.Expression.Function(
      "lookup",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            ast.Expression.Function("this"),
            ast.Expression.Value.String("..")
          )
        ),
        ast.Expression.Value.String("foo")
      )
    )

    assertEquals(result, ("", expectedResult).asRight[Parser.Error], s"Cannot parse '$source'")
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
