package handrail

import fs2._

import cats.Show
import cats.MonadThrow
import cats.data.OptionT
import cats.effect._
import cats.syntax.all._

import org.apache.commons.text.StringEscapeUtils
import org.typelevel.log4cats.slf4j.Slf4jLogger

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
object HelpersRegistry {

  // TODO pass it in the context/scope
  val logger = Slf4jLogger.getLogger[IO]

  object Render extends Helper {

    override def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {

      override def apply(ctx: Context): Context = {
        if (!positionalArgs.isEmpty) {
          val value = positionalArgs.head
          val newValue = value(ctx).value match {
            case value: Expression.Value.String => value
            case Expression.Value.Boolean(value) => Expression.Value.String(value.toString)
            case Expression.Value.Number(value) => Expression.Value.String(value.toString)
            case Expression.Value.Object(value) => Expression.Value.String(value.toString)
            case Expression.Value.Array(values) => Expression.Value.String(values.toString)
            case Expression.Value.Void => Expression.Value.String("")
          }

          ctx.withValue(newValue)
        } else {
          // TODO nononono
          throw new RuntimeException("argument 'value' not found")
        }
      }
    }
  }

  object Escape extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {

      override def apply(ctx: Context) = {

        if (!positionalArgs.isEmpty) {
          val value = positionalArgs.head
          val newValue = value(ctx).value match {
            case Expression.Value.String(value) => Expression.Value.String(StringEscapeUtils.escapeHtml4(value))
            case Expression.Value.Boolean(value) =>
              Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
            case Expression.Value.Number(value) =>
              Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
            case Expression.Value.Object(value) =>
              Expression.Value.String(StringEscapeUtils.escapeHtml4(value.toString))
            case Expression.Value.Array(values) =>
              Expression.Value.String(StringEscapeUtils.escapeHtml4(values.toString))
            case Expression.Value.Void => Expression.Value.String("")
          }
          ctx.copy(value = newValue)
        } else {
          // TODO nononono
          throw new RuntimeException("argument 'value' not found")
        }
      }
    }
  }

  object Lookup extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {

      override def apply(ctx: Context) = {

        if (positionalArgs.size == 2) {
          val iterator = positionalArgs.iterator
          val targetExp = iterator.next
          val propertyExp = iterator.next

          val target = targetExp(ctx)
          val property = propertyExp(ctx).value match {
            case Expression.Value.String(x) => x
            case x: Expression.Value => throw new RuntimeException(s"value of property ${x} is not a String")
          }

          def resolveValue(property: String, target: Context): Context = property match {
            case "." | "this" => target
            case ".." => target.parent.getOrElse(Context.void) // TODO get parent
            case key =>
              val nextValue = target.value match {
                case Expression.Value.Object(value) =>
                  // TODO use the missing helper
                  value.getOrElse(key, Expression.Value.Void)
                // TODO use the missing helper
                case other => Expression.Value.Void
              }
              target.child(nextValue)
          }

          val result = resolveValue(property, target)

          println(s"'${property}' of '${target}' is '${result}'")

          result

        } else {
          throw new RuntimeException("Lookup needs two arguments")
        }
      }
    }
  }

  // TODO Remove it
  object This extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {
      override def apply(ctx: Context) = ctx
    }
  }

  // {{#if foo.bar baz=qux}}foo{{else}}bar{{/if}} => render(if(posArgs = List(lookup(context, "foo.bar")), namedArgs = Map("baz", lookup(context, "qux")))
  object If extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {
      override def apply(ctx: Context): Context = {

        val valueExp = nominalArgs
          .get("value")
          .orElse(positionalArgs.headOption)
          .getOrElse(throw new RuntimeException("argument 'value' not found"))

        val value = valueExp(ctx).value match {
          case Expression.Value.Boolean(value) => value
          case Expression.Value.Void => false
          case _ => true
        }

        val bodyExp = nominalArgs
          .getOrElse("body", throw new RuntimeException("argument 'body' not found"))

        val elseExp = nominalArgs
          .getOrElse("else", Expression.Value.Void)

        if (value) {
          bodyExp(ctx)
        } else {
          elseExp(ctx)
        }
      }

    }
  }

  object Not extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {
      override def apply(ctx: Context): Context = {
        val valueExp = nominalArgs
          .get("value")
          .orElse(positionalArgs.headOption)
          .getOrElse(throw new RuntimeException("argument 'value' not found"))

        val newValue = valueExp(ctx).value match {
          case Expression.Value.Boolean(value) => Expression.Value.Boolean(!value)
          case Expression.Value.Void => Expression.Value.Boolean(true)
          case _ => Expression.Value.Boolean(false)
        }

        ctx.withValue(newValue)
      }
    }
  }

  object Unless extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {
      override def apply(ctx: Context): Context = {
        val valueExp = nominalArgs
          .get("value")
          .orElse(positionalArgs.headOption)
          .getOrElse(throw new RuntimeException("argument 'value' not found"))

        // TODO Can we return the expression wrapped somehow instead?
        val newValue = Not(List(valueExp), Map.empty)
        If(List(newValue), nominalArgs - "value")(ctx)
      }
    }
  }

  object Template extends Helper {
    def apply(
        positionalArgs: List[Expression],
        nominalArgs: Map[String, Expression]
    ) = new Expression.Function {
      override def apply(ctx: Context): Context = {

        if (positionalArgs.nonEmpty) {
          val values = positionalArgs.head
          val nestedExps: Iterable[Expression] = values(ctx).value match {
            case Expression.Value.Array(exps) => exps
            case other => throw new RuntimeException("The values must be an array")
          }

          // TODO Capacity hint
          val sb = new StringBuilder()
          nestedExps.foreach { exp =>
            exp(ctx).value match {
              case Expression.Value.String(value) => sb.append(value)
              case Expression.Value.Void =>
              case other => throw new RuntimeException("The template nested expressions must return string or void")
            }
          }

          ctx.withValue(Expression.Value.String(sb.toString))
        } else {
          throw new RuntimeException("The values must be an array")
        }
      }
    }
  }

  val empty = HelpersRegistry(Map.empty)
  val default = empty
    .withHelper("this", HelpersRegistry.This) // TODO implement with lookup . .
    .withHelper(".", HelpersRegistry.This) // TODO implement with lookup . .
    .withHelper("lookup", HelpersRegistry.Lookup)
    .withHelper("render", HelpersRegistry.Render)
    .withHelper("template", HelpersRegistry.Template)
    .withHelper("escape", HelpersRegistry.Escape)
    .withHelper("if", HelpersRegistry.If)
    .withHelper("not", HelpersRegistry.Not)
    .withHelper("unless", HelpersRegistry.Unless)

  implicit val showForHelpersRegistry: Show[HelpersRegistry] = Show.show { hr =>
    hr.helpers.keySet.mkString("[", ",", "]")
  }

}

case class HelpersRegistry(helpers: Map[String, Helper]) {
  def withHelper(name: String, helper: Helper): HelpersRegistry = copy(helpers + (name -> helper))
  def helper(helperName: String): Option[Helper] = helpers.get(helperName)
  def unsafeHelper(helperName: String): Helper = helper(helperName).get
  // .orElse(helpers.get("missing"))
  // .getOrElse(
  //   throw new IllegalStateException(s"helper '${helperName}' not found and missing helper is not registered")
  // )
}
