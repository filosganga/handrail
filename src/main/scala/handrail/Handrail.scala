package handrail
import fs2._

import cats.effect._
import cats.syntax.all._

import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.StructuredLogger
import cats.Applicative

import handrail.model._
/*
 *
 * The argument of the functions are by default context property, unless they are number, or boolean, or in ""
 *
 * {{{ foo.bar }}} => render(lookup(context, "foo.bar"))
 * {{{ loud foo.bar }}} => render(loud(lookup(context, "foo.bar"))))
 * {{ foo.bar }} => render(escape(lookup(this, "foo.bar")))
 * {{# my comment}} => render("")
 * }} my nice text {{ => render(" my nice text ")
 * {{> foo arg1 }} => render(partial(foo, arg1))
 * {{#if foo.bar baz=qux}}foo{{else}}bar{{/if}} => render(if(posArgs = List(lookup(context, "foo.bar")), namedArgs = Map("baz", lookup(context, "qux")))
 *
 */
object Handrail {

  def parse(source: String, helpersRegistry: HelpersRegistry): Either[HandrailParseError, Template] = {
    val expression = new HandlebarsParser(helpersRegistry).TemplateP
      .parseAll(source)
      .onError { e =>
        println(s"Parser error: ${e.expected}")
        e.asLeft
      }
      .leftMap(e => new HandrailParseError(e.toString))

    expression.map { exp =>
      new Template {
        def apply(data: Expression.Value): Either[HandrailExecutionError, String] = {
          exp(Context(data)).value match {
            case Expression.Value.String(value) => value.asRight[HandrailExecutionError]
            case other => HandrailExecutionError(s"$other is not a valid String").asLeft
          }
        }
      }
    }
  }

  // def eval(
  //     expression: Expression,
  //     data: Expression.Value,
  //     helpersRegistry: HelpersRegistry
  // ): Expression.Value = {

  //   def eval(expression: Expression, data: Expression.Value): Expression.Value = expression match {
  //     case value: Expression.Value => value
  //     case Expression.Function(name, posArgs, namedArgs) =>
  //       helpersRegistry.helpers
  //         .get(name)
  //         .map { helper =>
  //           helper(
  //             positionalArgs = posArgs,
  //             nominalArgs = namedArgs,
  //             data = data,
  //             eval = eval _
  //           )
  //         }
  //         .getOrElse {
  //           if (posArgs.isEmpty && namedArgs.isEmpty) {
  //             Helper.Lookup(List(data, model.Expression.Value.String(name)), Map.empty, data, eval _)
  //           } else {
  //             // TODO Use missing helper
  //             model.Expression.Value.Void
  //           }
  //         }
  //   }

  //   eval(expression, data)
  // }

}
