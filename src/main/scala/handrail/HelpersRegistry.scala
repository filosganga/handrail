package handrail
import ast._
import fs2._

import cats.Show
import cats.data.OptionT
import cats.effect._
import cats.syntax.all._

import org.apache.commons.text.StringEscapeUtils
import org.typelevel.log4cats.slf4j.Slf4jLogger
import cats.MonadThrow

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
trait Helper[F[_]] {
  def apply(
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => F[Expression.Value],
      output: Output[F]
  ): F[Expression.Value]
}

object Helper {

  // TODO pass in the context/scope
  val logger = Slf4jLogger.getLogger[IO]

  def render[F[_]: MonadThrow]: Helper[F] = new Helper[F] {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression],
        data: Expression.Value,
        eval: (Expression, Expression.Value) => F[Expression.Value],
        output: Output[F]
    ): F[Expression.Value] = {
      val value = MonadThrow[F].fromOption(
        nominalArgs
          .get("value")
          .orElse(positionalArgs.headOption),
        throw new RuntimeException("argument 'value' not found")
      )

      value
        .flatMap(exp => eval(exp, data))
        .flatMap {
          case Expression.Value.String(value) => value.pure[F]
          case Expression.Value.Boolean(value) => value.toString.pure[F]
          case Expression.Value.Number(value) => value.toString.pure[F]
          case Expression.Value.Object(value) => value.toString.pure[F]
          case Expression.Value.Void => "".pure[F]
        }
        .flatMap(value => output.append(value.toString))
        .as(Expression.Value.Void)
    }
  }

  def escape[F[_]: MonadThrow]: Helper[F] = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => F[Expression.Value],
      output: Output[F]
  ) => {
    val value = MonadThrow[F].fromOption(
      nominalArgs
        .get("value")
        .orElse(positionalArgs.headOption),
      throw new RuntimeException("argument 'value' not found")
    )

    value
      .flatMap(exp => eval(exp, data))
      .flatMap {
        case Expression.Value.String(value) => value.pure[F]
        case Expression.Value.Boolean(value) => value.toString.pure[F]
        case Expression.Value.Number(value) => value.toString.pure[F]
        case Expression.Value.Object(value) => value.toString.pure[F]
        case Expression.Value.Void => "".pure[F]
      }
      .flatMap { value =>
        MonadThrow[F].catchNonFatal(StringEscapeUtils.escapeHtml4(value)).map(Expression.Value.String(_))
      }
  }

  def lookup[F[_]: MonadThrow]: Helper[F] = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => F[Expression.Value],
      output: Output[F]
  ) => {
    val dataF = MonadThrow[F]
      .fromOption(
        nominalArgs
          .get("data")
          .orElse(positionalArgs.headOption),
        throw new RuntimeException("argument 'data' not found")
      )
      .flatMap(exp => eval(exp, data))

    val propertyF: F[String] = MonadThrow[F]
      .fromOption(
        nominalArgs
          .get("property")
          .orElse(positionalArgs.get(1)),
        throw new RuntimeException("argument 'property' not found")
      )
      .flatMap(exp => eval(exp, data))
      .flatMap {
        case Expression.Value.String(x) => x.pure[F]
        case x: Expression.Value =>
          MonadThrow[F].raiseError(new RuntimeException(s"value of property ${x} is not a String"))
      }

    (dataF, propertyF).mapN { (data, property) =>
      property match {
        case "." => data.pure[F]
        case ".." => data.pure[F] // TODO the parent
        case key =>
          data match {
            case Expression.Value.Object(value) =>
              // TODO use the missing helper
              value.get(key).traverse(x => eval(x, data)).map { opt =>
                opt.fold[Expression.Value](Expression.Value.Void)(identity)
              }
            // TODO use the missing helper
            case other => MonadThrow[F].raiseError[Expression.Value](new RuntimeException(s"${data} is not an object"))
          }
        // TODO handle other cases
      }
    }.flatten
  }

  // {{#if foo.bar baz=qux}}foo{{else}}bar{{/if}} => render(if(posArgs = List(lookup(context, "foo.bar")), namedArgs = Map("baz", lookup(context, "qux")))
  def `if`[F[_]: MonadThrow]: Helper[F] = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => F[Expression.Value],
      output: Output[F]
  ) => {
    val valueF = MonadThrow[F]
      .fromOption(
        nominalArgs
          .get("value")
          .orElse(positionalArgs.headOption),
        throw new RuntimeException("argument 'value' not found")
      )
      .flatMap(exp => eval(exp, data))
      .flatMap {
        case Expression.Value.Boolean(value) => value.pure[F]
        case Expression.Value.Void => false.pure[F]
        case _ => true.pure[F]
      }

    val bodyF = MonadThrow[F]
      .fromOption(
        nominalArgs
          .get("body"),
        throw new RuntimeException("argument 'body' not found")
      )
      .flatMap(exp => eval(exp, data))

    val elseF =
      nominalArgs
        .getOrElse("else", Expression.Value.Void)
        .pure[F]
        .flatMap(exp => eval(exp, data))

    valueF.ifM(
      bodyF,
      elseF
    )
  }

  def not[F[_]: MonadThrow]: Helper[F] = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => F[Expression.Value],
      output: Output[F]
  ) => {
    MonadThrow[F]
      .fromOption(
        nominalArgs
          .get("value")
          .orElse(positionalArgs.headOption),
        throw new RuntimeException("argument 'value' not found")
      )
      .flatMap(exp => eval(exp, data))
      .flatMap {
        case Expression.Value.Boolean(value) => value.pure[F]
        case Expression.Value.Void => false.pure[F]
        case _ => true.pure[F]
      }
      .map(x => Expression.Value.Boolean(!x))
  }

  def unless[F[_]: MonadThrow]: Helper[F] = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => F[Expression.Value],
      output: Output[F]
  ) => {
    val valueF = MonadThrow[F]
      .fromOption(
        nominalArgs
          .get("value")
          .orElse(positionalArgs.headOption),
        throw new RuntimeException("argument 'value' not found")
      )
      .map(exp => Expression.Function("not", List(exp)))

    valueF.flatMap { value =>
      eval(Expression.Function("if", List(value), nominalArgs - "value"), data)
    }
  }

}

case class HelpersRegistry[F[_]](helpers: Map[String, Helper[F]]) {
  def withHelper(name: String, helper: Helper[F]): HelpersRegistry[F] = copy(helpers + (name -> helper))
}

object HelpersRegistry {
  def empty[F[_]: MonadThrow] = HelpersRegistry[F](Map.empty)
  def default[F[_]: MonadThrow] = empty
    .withHelper("lookup", Helper.lookup[F])
    .withHelper("render", Helper.render)
    .withHelper("escape", Helper.escape)
    .withHelper("if", Helper.`if`)
    .withHelper("not", Helper.not)
    .withHelper("unless", Helper.unless)

  implicit def showForHelpersRegistry[F[_]]: Show[HelpersRegistry[F]] = Show.show { hr =>
    hr.helpers.keySet.mkString("[", ",", "]")
  }
}
