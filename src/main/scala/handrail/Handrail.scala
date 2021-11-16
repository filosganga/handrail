package handrail
import ast._
import fs2._

import cats.effect._
import cats.syntax.all._

import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.StructuredLogger
import cats.Applicative

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

  def parse(source: String): Either[Throwable, Expression] = {
    HandlebarsParser.TemplateP
      .parseAll(source)
      .leftMap(e => new RuntimeException(e.toString))
  }

  def eval(
      expression: Expression,
      data: Expression.Value,
      helpersRegistry: HelpersRegistry
  ): Expression.Value = {

    def eval(expression: Expression, data: Expression.Value): Expression.Value = expression match {
      case value: Expression.Value => value
      case Expression.Function(name, posArgs, namedArgs) =>
        helpersRegistry.helpers
          .get(name)
          .map { helper =>
            helper(
              positionalArgs = posArgs,
              nominalArgs = namedArgs,
              data = data,
              eval = eval _
            )
          }
          .getOrElse {
            if (posArgs.isEmpty && namedArgs.isEmpty) {
              Helper.Lookup(List(data, ast.Expression.Value.String(name)), Map.empty, data, eval _)
            } else {
              // TODO Use missing helper
              ast.Expression.Value.Void
            }
          }
    }

    eval(expression, data)
  }

}
