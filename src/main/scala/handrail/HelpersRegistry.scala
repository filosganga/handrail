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
trait Helper {
  def apply(
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ): Expression.Value
}

object Helper {

  // TODO pass in the context/scope
  val logger = Slf4jLogger.getLogger[IO]

  object Render extends Helper {

    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression],
        data: Expression.Value,
        eval: (Expression, Expression.Value) => Expression.Value
    ): Expression.Value = {

      if (!positionalArgs.isEmpty) {
        val value = positionalArgs.head
        eval(value, data) match {
          case value: Expression.Value.String => value
          case Expression.Value.Boolean(value) => Expression.Value.String(value.toString)
          case Expression.Value.Number(value) => Expression.Value.String(value.toString)
          case Expression.Value.Object(value) => Expression.Value.String(value.toString)
          case Expression.Value.Array(values) => Expression.Value.String(values.toString)
          case Expression.Value.Void => Expression.Value.String("")
        }
      } else {
        throw new RuntimeException("argument 'value' not found")
      }
    }
  }

  object Escape extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression],
        data: Expression.Value,
        eval: (Expression, Expression.Value) => Expression.Value
    ): Expression.Value = {

      if (!positionalArgs.isEmpty) {
        val value = positionalArgs.head
        eval(value, data) match {
          case Expression.Value.String(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value))
          case Expression.Value.Boolean(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
          case Expression.Value.Number(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
          case Expression.Value.Object(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
          case Expression.Value.Array(values) => Expression.Value.String(StringEscapeUtils.escapeHtml4(values.toString))
          case Expression.Value.Void => Expression.Value.String("")
        }
      } else {
        throw new RuntimeException("argument 'value' not found")
      }
    }
  }

  object Lookup extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression],
        data: Expression.Value,
        eval: (Expression, Expression.Value) => Expression.Value
    ): Expression.Value = {

      if (positionalArgs.size == 2) {
        val iterator = positionalArgs.iterator
        val targetExp = iterator.next
        val propertyExp = iterator.next

        val target = eval(targetExp, data)

        val property = eval(propertyExp, data) match {
          case Expression.Value.String(x) => x
          case x: Expression.Value => throw new RuntimeException(s"value of property ${x} is not a String")
        }

        property match {
          case "." | "this" => target
          // TODO get parent
          case ".." => target
          case key =>
            target match {
              case Expression.Value.Object(value) =>
                // TODO use the missing helper
                value.getOrElse(key, Expression.Value.Void)
              // TODO use the missing helper
              case other => Expression.Value.Void
            }
        }

      } else {
        throw new RuntimeException("Lookup needs two arguments")
      }
    }
  }

  object This extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression],
        data: Expression.Value,
        eval: (Expression, Expression.Value) => Expression.Value
    ): Expression.Value = data
  }

  // {{#if foo.bar baz=qux}}foo{{else}}bar{{/if}} => render(if(posArgs = List(lookup(context, "foo.bar")), namedArgs = Map("baz", lookup(context, "qux")))
  val `if`: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ) => {

    val valueExp = nominalArgs
      .get("value")
      .orElse(positionalArgs.headOption)
      .getOrElse(throw new RuntimeException("argument 'value' not found"))

    val value = eval(valueExp, data) match {
      case Expression.Value.Boolean(value) => value
      case Expression.Value.Void => false
      case _ => true
    }

    val bodyExp = nominalArgs
      .getOrElse("body", throw new RuntimeException("argument 'body' not found"))

    val elseExp = nominalArgs
      .getOrElse("else", Expression.Value.Void)

    // TODO Can we return the expression without evaluating it?
    if (value) {
      eval(bodyExp, data)
    } else {
      eval(elseExp, data)
    }
  }

  val not: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ) => {
    val valueExp = nominalArgs
      .get("value")
      .orElse(positionalArgs.headOption)
      .getOrElse(throw new RuntimeException("argument 'value' not found"))

    // TODO Can we return the expression wrapped somehow instead?
    eval(valueExp, data) match {
      case Expression.Value.Boolean(value) => Expression.Value.Boolean(!value)
      case Expression.Value.Void => Expression.Value.Boolean(true)
      case _ => Expression.Value.Boolean(false)
    }
  }

  val unless: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ) => {
    val valueExp = nominalArgs
      .get("value")
      .orElse(positionalArgs.headOption)
      .getOrElse(throw new RuntimeException("argument 'value' not found"))

    // TODO Can we return the expression wrapped somehow instead?
    val newValue = Expression.Function("not", List(valueExp))
    eval(Expression.Function("if", List(newValue), nominalArgs - "value"), data)
  }

  object Template extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression],
        data: Expression.Value,
        eval: (Expression, Expression.Value) => Expression.Value
    ): Expression.Value = {

      if (positionalArgs.nonEmpty) {
        val values = positionalArgs.head
        val nestedExps: Iterable[Expression] = eval(values, data) match {
          case Expression.Value.Array(exps) => exps
          case other => throw new RuntimeException("The values must be an array")
        }

        // TODO Capacity hint
        val sb = new StringBuilder()
        nestedExps.foreach { exp =>
          eval(exp, data) match {
            case Expression.Value.String(value) => sb.append(value)
            case Expression.Value.Void =>
            case other => throw new RuntimeException("The template nested expressions must return string or void")
          }
        }

        Expression.Value.String(sb.toString)
      } else {
        throw new RuntimeException("The values must be an array")
      }
    }
  }
}

case class HelpersRegistry(helpers: Map[String, Helper]) {
  def withHelper(name: String, helper: Helper): HelpersRegistry = copy(helpers + (name -> helper))
  def helper(helperName: String): Helper = helpers
    .get(helperName)
    .orElse(helpers.get("missing"))
    .getOrElse(throw new IllegalStateException("missing helper is not registered"))
}

object HelpersRegistry {
  val empty = HelpersRegistry(Map.empty)
  val default = empty
    .withHelper("this", Helper.This)
    .withHelper("lookup", Helper.Lookup)
    .withHelper("render", Helper.Render)
    .withHelper("template", Helper.Template)
    .withHelper("escape", Helper.Escape)
    .withHelper("if", Helper.`if`)
    .withHelper("not", Helper.not)
    .withHelper("unless", Helper.unless)

  implicit val showForHelpersRegistry: Show[HelpersRegistry] = Show.show { hr =>
    hr.helpers.keySet.mkString("[", ",", "]")
  }
}
