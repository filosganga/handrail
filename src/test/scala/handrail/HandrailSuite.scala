package handrail

import cats.syntax.all._
import cats.parse._
import cats.effect._

class HandrailSuite extends munit.CatsEffectSuite {

  val stringBuilderR = ResourceFixture(Resource.make(IO(new StringBuilder))(sb => IO(sb.clear)))

  test("it should render itself") {
    val data = ast.Expression.Value.String("foo")
    val expression = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            data,
            ast.Expression.Value.String(".")
          ),
          Map.empty
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("foo"))
  }

  test("it should render a property") {
    val data = ast.Expression.Value.Object(Map("foo" -> ast.Expression.Value.String("bar")))
    val expression = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "lookup",
          List(
            data,
            ast.Expression.Value.String("foo")
          ),
          Map.empty
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("bar"))
  }

  test("it should escape HTML") {
    val data = ast.Expression.Value.Object(Map("foo" -> ast.Expression.Value.String("perché")))
    val expression = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "escape",
          List(
            ast.Expression.Function(
              "lookup",
              List(
                data,
                ast.Expression.Value.String("foo")
              ),
              Map.empty
            )
          ),
          Map.empty
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("perch&eacute;"))
  }

  test("{{#if}} should evaluate body when true") {
    val data = ast.Expression.Value.Object(Map("foo" -> ast.Expression.Value.String("perché")))
    val expression = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "if",
          List(
            ast.Expression.Value.Boolean(true)
          ),
          Map(
            "body" -> ast.Expression.Function(
              "render",
              List(ast.Expression.Value.String("foo"))
            )
          )
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("foo"))
  }

  stringBuilderR.test("{{#if}} should evaluate else when false") { sb =>
    val data = ast.Expression.Value.Object(Map("foo" -> ast.Expression.Value.String("perché")))
    val expression = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "if",
          List(
            ast.Expression.Value.Boolean(false)
          ),
          Map(
            "body" -> ast.Expression.Function(
              "render",
              List(ast.Expression.Value.String("foo"))
            ),
            "else" -> ast.Expression.Function(
              "render",
              List(ast.Expression.Value.String("bar"))
            )
          )
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("bar"))
  }

  test("{{#unless}} should evaluate body when false") {
    val data = ast.Expression.Value.Object(Map("foo" -> ast.Expression.Value.String("perché")))
    val expression = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "unless",
          List(
            ast.Expression.Value.Boolean(false)
          ),
          Map(
            "body" -> ast.Expression.Function(
              "render",
              List(ast.Expression.Value.String("foo"))
            ),
            "else" -> ast.Expression.Function(
              "render",
              List(ast.Expression.Value.String("bar"))
            )
          )
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("foo"))
  }

  test("{{#unless}} should evaluate else when true") {
    val data = ast.Expression.Value.Object(Map("foo" -> ast.Expression.Value.String("perché")))
    val expression = ast.Expression.Function(
      "render",
      List(
        ast.Expression.Function(
          "unless",
          List(
            ast.Expression.Value.Boolean(true)
          ),
          Map(
            "body" -> ast.Expression.Function(
              "render",
              List(ast.Expression.Value.String("foo"))
            ),
            "else" -> ast.Expression.Function(
              "render",
              List(ast.Expression.Value.String("bar"))
            )
          )
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("bar"))
  }

  test("template should concatenate expressions values") {
    val data = ast.Expression.Value.Object(Map.empty)
    val expression = ast.Expression.Function(
      "template",
      List(
        ast.Expression.Value.Array(
          List(
            ast.Expression.Function(
              "render",
              List(
                ast.Expression.Value.String("foo")
              ),
              Map.empty
            ),
            ast.Expression.Function(
              "render",
              List(
                ast.Expression.Value.String("bar")
              ),
              Map.empty
            )
          )
        )
      ),
      Map.empty
    )

    val result = Handrail
      .eval(
        expression,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("foobar"))
  }

  test("Handrail should compile and fill a simple template") {
    val data = ast.Expression.Value.Object(Map("name" -> ast.Expression.Value.String("John")))

    val source = """Hello, {{name}}"""

    val template = HandlebarsParser.TemplateP.parseAll(source).getOrElse(throw new RuntimeException)

    val result = Handrail
      .eval(
        template,
        data,
        HelpersRegistry.default
      )

    assertEquals(result, ast.Expression.Value.String("Hello, John"))
  }
}
