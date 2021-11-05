package handrail
import ast._
import fs2._

import cats.effect._
import cats.syntax.all._

import org.typelevel.log4cats.slf4j.Slf4jLogger

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

  // TODO pass in the context/scope
  val logger = Slf4jLogger.getLogger[IO]

  def eval(
      expression: Expression,
      data: Expression.Value,
      output: StringBuilder,
      helpersRegistry: HelpersRegistry = HelpersRegistry.default
  ): IO[Expression.Value] = {
    expression match {
      case value: Expression.Value => value.pure[IO]
      case Expression.Function(name, posArgs, namedArgs) =>
        logger.info(s"Calling helper: ${name} with posArgs: ${posArgs} namedArgs: ${namedArgs}") *>
          IO.fromOption(helpersRegistry.helpers.get(name))(
            new RuntimeException(show"Helper with name ${name} does not exist in the HelperRegistry ${helpersRegistry}")
          ).flatMap { helper =>
            helper(
              positionalArgs = posArgs,
              nominalArgs = namedArgs,
              data = data,
              evaluator = (exp, data) => eval(exp, data, output, helpersRegistry),
              output = output
            )
          }.flatTap(result =>
            logger.info(
              s"Helper: ${name} with posArgs: ${posArgs} namedArgs: ${namedArgs} returned: ${result} output: ${output.toString}"
            )
          )
    }
  }

}
