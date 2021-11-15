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

  val render: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ) => {
    val value = nominalArgs
      .get("value")
      .orElse(positionalArgs.headOption)
      .getOrElse(throw new RuntimeException("argument 'value' not found"))

    eval(value, data) match {
      case value: Expression.Value.String => value
      case Expression.Value.Boolean(value) => Expression.Value.String(value.toString)
      case Expression.Value.Number(value) => Expression.Value.String(value.toString)
      case Expression.Value.Object(value) => Expression.Value.String(value.toString)
      case Expression.Value.Void => Expression.Value.String("")
    }
  }

  val escape: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ) => {
    val value = nominalArgs
      .get("value")
      .orElse(positionalArgs.headOption)
      .getOrElse(throw new RuntimeException("argument 'value' not found"))

    eval(value, data) match {
      case Expression.Value.String(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value))
      case Expression.Value.Boolean(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
      case Expression.Value.Number(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
      case Expression.Value.Object(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
      case Expression.Value.Void => Expression.Value.String("")
    }
  }

  val lookup: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ) => {
    val targetExp = nominalArgs
      .get("target")
      .orElse(positionalArgs.headOption)
      .getOrElse(throw new RuntimeException("argument 'target' not found"))

    val propertyExp = nominalArgs
      .get("property")
      .orElse(positionalArgs.get(1))
      .getOrElse(throw new RuntimeException("argument 'property' not found"))

    val property = eval(propertyExp, data) match {
      case Expression.Value.String(x) => x
      case x: Expression.Value => throw new RuntimeException(s"value of property ${x} is not a String")
    }

    // TODO Can we return the expression instead of value from the helper?
    val target = eval(targetExp, data)

    property match {
      case "." | "this" => target
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
  }

  val `this`: Helper = (
      positionalArgs: List[Expression],
      nominalArgs: Map[String, Expression],
      data: Expression.Value,
      eval: (Expression, Expression.Value) => Expression.Value
  ) => data

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

}

case class HelpersRegistry(helpers: Map[String, Helper]) {
  def withHelper(name: String, helper: Helper): HelpersRegistry = copy(helpers + (name -> helper))
}

object HelpersRegistry {
  val empty = HelpersRegistry(Map.empty)
  val default = empty
    .withHelper("this", Helper.lookup)
    .withHelper("lookup", Helper.lookup)
    .withHelper("render", Helper.render)
    .withHelper("escape", Helper.escape)
    .withHelper("if", Helper.`if`)
    .withHelper("not", Helper.not)
    .withHelper("unless", Helper.unless)

  implicit val showForHelpersRegistry: Show[HelpersRegistry] = Show.show { hr =>
    hr.helpers.keySet.mkString("[", ",", "]")
  }
}
