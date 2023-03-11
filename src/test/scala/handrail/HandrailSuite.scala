package handrail

import cats.syntax.all._
import cats.parse._
import cats.effect._

import handrail.model._

class HandrailSuite extends munit.CatsEffectSuite {

  val stringBuilderR = ResourceFixture(Resource.make(IO(new StringBuilder))(sb => IO(sb.clear)))

  test("Handrail should render {{this}}") {
    val result =
      Handrail.parse("{{this}}", HelpersRegistry.default).flatMap { template =>
        template(Expression.Value.String("foo")).leftWiden[HandrailError]
      }

    assertEquals(result, "foo".asRight[HandrailError])
  }

  test("Handrail should render {{.}}") {
    val result =
      Handrail.parse("{{.}}", HelpersRegistry.default).flatMap { template =>
        template(Expression.Value.String("foo")).leftWiden[HandrailError]
      }

    assertEquals(result, "foo".asRight[HandrailError])
  }

  test("Handrail should render {{foo}}") {
    val result =
      Handrail.parse("{{foo}}", HelpersRegistry.default).flatMap { template =>
        template(Expression.Value.Object(Map("foo" -> Expression.Value.String("bar")))).leftWiden[HandrailError]
      }

    assertEquals(result, "bar".asRight[HandrailError])
  }

  test("Handrail should render {{loud foo}}") {

    val helpers = HelpersRegistry.default.withHelper("loud", Loud)
    val result =
      Handrail.parse("{{loud foo}}", helpers).flatMap { template =>
        template(Expression.Value.Object(Map("foo" -> Expression.Value.String("bar")))).leftWiden[HandrailError]
      }

    assertEquals(result, "BAR".asRight[HandrailError])
  }

  test("Handrail should render {{foo.bar}}") {
    val result =
      Handrail.parse("{{foo.bar}}", HelpersRegistry.default).flatMap { template =>
        template(
          Expression.Value.Object(Map("foo" -> Expression.Value.Object(Map("bar" -> Expression.Value.String("baz")))))
        ).leftWiden[HandrailError]
      }

    assertEquals(result, "baz".asRight[HandrailError])
  }

//   test("it should escape HTML") {
//     val data = model.Expression.Value.Object(Map("foo" -> model.Expression.Value.String("perché")))
//     val expression = model.Expression.Function(
//       "render",
//       List(
//         model.Expression.Function(
//           "escape",
//           List(
//             model.Expression.Function(
//               "lookup",
//               List(
//                 data,
//                 model.Expression.Value.String("foo")
//               ),
//               Map.empty
//             )
//           ),
//           Map.empty
//         )
//       ),
//       Map.empty
//     )

//     val result = Handrail
//       .eval(
//         expression,
//         data,
//         HelpersRegistry.default
//       )

//     assertEquals(result, model.Expression.Value.String("perch&eacute;"))
//   }

//   test("{{#if}} should evaluate body when true") {
//     val data = model.Expression.Value.Object(Map("foo" -> model.Expression.Value.String("perché")))
//     val expression = model.Expression.Function(
//       "render",
//       List(
//         model.Expression.Function(
//           "if",
//           List(
//             model.Expression.Value.Boolean(true)
//           ),
//           Map(
//             "body" -> model.Expression.Function(
//               "render",
//               List(model.Expression.Value.String("foo"))
//             )
//           )
//         )
//       ),
//       Map.empty
//     )

//     val result = Handrail
//       .eval(
//         expression,
//         data,
//         HelpersRegistry.default
//       )

//     assertEquals(result, model.Expression.Value.String("foo"))
//   }

//   stringBuilderR.test("{{#if}} should evaluate else when false") { sb =>
//     val data = model.Expression.Value.Object(Map("foo" -> model.Expression.Value.String("perché")))
//     val expression = model.Expression.Function(
//       "render",
//       List(
//         model.Expression.Function(
//           "if",
//           List(
//             model.Expression.Value.Boolean(false)
//           ),
//           Map(
//             "body" -> model.Expression.Function(
//               "render",
//               List(model.Expression.Value.String("foo"))
//             ),
//             "else" -> model.Expression.Function(
//               "render",
//               List(model.Expression.Value.String("bar"))
//             )
//           )
//         )
//       ),
//       Map.empty
//     )

//     val result = Handrail
//       .eval(
//         expression,
//         data,
//         HelpersRegistry.default
//       )

//     assertEquals(result, model.Expression.Value.String("bar"))
//   }

//   test("{{#unless}} should evaluate body when false") {
//     val data = model.Expression.Value.Object(Map("foo" -> model.Expression.Value.String("perché")))
//     val expression = model.Expression.Function(
//       "render",
//       List(
//         model.Expression.Function(
//           "unless",
//           List(
//             model.Expression.Value.Boolean(false)
//           ),
//           Map(
//             "body" -> model.Expression.Function(
//               "render",
//               List(model.Expression.Value.String("foo"))
//             ),
//             "else" -> model.Expression.Function(
//               "render",
//               List(model.Expression.Value.String("bar"))
//             )
//           )
//         )
//       ),
//       Map.empty
//     )

//     val result = Handrail
//       .eval(
//         expression,
//         data,
//         HelpersRegistry.default
//       )

//     assertEquals(result, model.Expression.Value.String("foo"))
//   }

//   test("{{#unless}} should evaluate else when true") {
//     val data = model.Expression.Value.Object(Map("foo" -> model.Expression.Value.String("perché")))
//     val expression = model.Expression.Function(
//       "render",
//       List(
//         model.Expression.Function(
//           "unless",
//           List(
//             model.Expression.Value.Boolean(true)
//           ),
//           Map(
//             "body" -> model.Expression.Function(
//               "render",
//               List(model.Expression.Value.String("foo"))
//             ),
//             "else" -> model.Expression.Function(
//               "render",
//               List(model.Expression.Value.String("bar"))
//             )
//           )
//         )
//       ),
//       Map.empty
//     )

//     val result = Handrail
//       .eval(
//         expression,
//         data,
//         HelpersRegistry.default
//       )

//     assertEquals(result, model.Expression.Value.String("bar"))
//   }

//   test("template should concatenate expressions values") {
//     val data = model.Expression.Value.Object(Map.empty)
//     val expression = model.Expression.Function(
//       "template",
//       List(
//         model.Expression.Value.Array(
//           List(
//             model.Expression.Function(
//               "render",
//               List(
//                 model.Expression.Value.String("foo")
//               ),
//               Map.empty
//             ),
//             model.Expression.Function(
//               "render",
//               List(
//                 model.Expression.Value.String("bar")
//               ),
//               Map.empty
//             )
//           )
//         )
//       ),
//       Map.empty
//     )

//     val result = Handrail
//       .eval(
//         expression,
//         data,
//         HelpersRegistry.default
//       )

//     assertEquals(result, model.Expression.Value.String("foobar"))
//   }

//   test("Handrail should compile and fill a simple template") {
//     val data = model.Expression.Value.Object(Map("name" -> model.Expression.Value.String("John")))

//     val source = """Hello, {{name}}"""

//     val template = HandlebarsParser.TemplateP.parseAll(source).getOrElse(throw new RuntimeException)

//     val result = Handrail
//       .eval(
//         template,
//         data,
//         HelpersRegistry.default
//       )

//     assertEquals(result, model.Expression.Value.String("Hello, John"))
//   }
}
