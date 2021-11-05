package handrail
import ast._
import fs2._

import cats.Show
import cats.data.OptionT
import cats.effect._
import cats.syntax.all._

import org.apache.commons.text.StringEscapeUtils
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
trait Helper {
  def apply(
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      evaluator: (Expression, Expression.Value) => IO[Expression.Value],
      output: StringBuilder
  ): IO[Expression.Value]
}

object Helper {

  // TODO pass in the context/scope
  val logger = Slf4jLogger.getLogger[IO]

  val render: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      evaluator: (Expression, Expression.Value) => IO[Expression.Value],
      output: StringBuilder
  ) => {
    val value = IO.fromOption(
      nominalArgs
        .get("value")
        .orElse(positionalArgs.headOption)
    )(throw new RuntimeException("argument 'value' not found"))

    value
      .flatMap(exp => evaluator(exp, data))
      .flatMap {
        case Expression.Value.String(value) => value.pure[IO]
        case Expression.Value.Boolean(value) => value.toString.pure[IO]
        case Expression.Value.Number(value) => value.toString.pure[IO]
        case Expression.Value.Object(value) => value.toString.pure[IO]
        case Expression.Value.Void => "".pure[IO]
      }
      .flatMap(value => IO(output.append(value.toString)).as(Expression.Value.Void))
  }

  val escape: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      evaluator: (Expression, Expression.Value) => IO[Expression.Value],
      output: StringBuilder
  ) => {
    val value = IO.fromOption(
      nominalArgs
        .get("value")
        .orElse(positionalArgs.headOption)
    )(throw new RuntimeException("argument 'value' not found"))

    value
      .flatMap(exp => evaluator(exp, data))
      .flatMap {
        case Expression.Value.String(value) => value.pure[IO]
        case Expression.Value.Boolean(value) => value.toString.pure[IO]
        case Expression.Value.Number(value) => value.toString.pure[IO]
        case Expression.Value.Object(value) => value.toString.pure[IO]
        case Expression.Value.Void => "".pure[IO]
      }
      .flatMap { value =>
        IO(StringEscapeUtils.escapeHtml4(value)).map(Expression.Value.String(_))
      }
  }

  val lookup: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      evaluator: (Expression, Expression.Value) => IO[Expression.Value],
      output: StringBuilder
  ) => {
    val dataF = IO
      .fromOption(
        nominalArgs
          .get("data")
          .orElse(positionalArgs.headOption)
      )(throw new RuntimeException("argument 'data' not found"))
      .flatMap(exp => evaluator(exp, data))
      .flatTap(x => logger.debug(s"data = ${x}"))

    val propertyF: IO[String] = IO
      .fromOption(
        nominalArgs
          .get("property")
          .orElse(positionalArgs.get(1))
      )(throw new RuntimeException("argument 'property' not found"))
      .flatMap(exp => evaluator(exp, data))
      .flatMap {
        case Expression.Value.String(x) => x.pure[IO]
        case x: Expression.Value => IO.raiseError(new RuntimeException(s"value of property ${x} is not a String"))
      }
      .flatTap(x => logger.debug(s"property = ${x}"))

    (dataF, propertyF).mapN { (data, property) =>
      property match {
        case "." => data.pure[IO]
        case ".." => data.pure[IO] // TODO the parent
        case key =>
          data match {
            case Expression.Value.Object(value) =>
              // TODO use the missing helper
              value.get(key).traverse(x => evaluator(x, data)).map { opt =>
                opt.fold[Expression.Value](Expression.Value.Void)(identity)
              }
            // TODO use the missing helper
            case other => IO.raiseError[Expression.Value](new RuntimeException(s"${data} is not an object"))
          }
        // TODO handle other cases
      }
    }.flatten
  }

}

case class HelpersRegistry(helpers: Map[String, Helper]) {
  def withHelper(name: String, helper: Helper): HelpersRegistry = copy(helpers + (name -> helper))
}

object HelpersRegistry {
  val empty = HelpersRegistry(Map.empty)
  val default = empty
    .withHelper("lookup", Helper.lookup)
    .withHelper("render", Helper.render)
    .withHelper("escape", Helper.escape)

  implicit val showForHelpersRegistry: Show[HelpersRegistry] = Show.show { hr =>
    hr.helpers.keySet.mkString("[", ",", "]")
  }
}
