package handrail

import handrail.model._

object Loud extends model.Helper {
  def apply(positionalArgs: List[Expression], nominalArgs: Map[String, Expression]): Expression.Function = {
    new Expression.Function {
      def apply(ctx: Context): Context = {
        if (positionalArgs.size == 1) {
          val newValue = positionalArgs(1)(ctx).value match {
            case Expression.Value.String(value) => Expression.Value.String(value.toUpperCase)
            case Expression.Value.Boolean(value) => Expression.Value.String(value.toString.toUpperCase)
            case Expression.Value.Number(value) => Expression.Value.String(value.toString.toUpperCase)
            case other => throw new RuntimeException("${other} not supported")
          }

          ctx.withValue(newValue)
        } else {
          throw new RuntimeException("loud should have 1 positional")
        }
      }
    }
  }
}
