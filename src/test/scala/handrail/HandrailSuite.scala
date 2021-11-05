package handrail

import cats.syntax.all._
import cats.parse._
import cats.effect._

class HandrailSuite extends munit.CatsEffectSuite {

  val stringBuilderR = ResourceFixture(Resource.make(IO(new StringBuilder))(sb => IO(sb.clear)))
  /*
   * template: {{.}}
   * context: "foo"
   * result: "foo"
   */
  stringBuilderR.test("it should render itself") { sb =>
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

    Handrail
      .eval(
        expression,
        data,
        sb
      )
      .flatMap { _ =>
        IO(sb.toString)
      }
      .assertEquals("foo")
  }

  stringBuilderR.test("it should render a property") { sb =>
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

    Handrail
      .eval(
        expression,
        data,
        sb
      )
      .flatMap { _ =>
        IO(sb.toString)
      }
      .assertEquals("bar")
  }

  stringBuilderR.test("it should escape HTML") { sb =>
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

    Handrail
      .eval(
        expression,
        data,
        sb
      )
      .flatMap { _ =>
        IO(sb.toString)
      }
      .assertEquals("perch&eacute;")
  }
}
