package handrail

import scala.collection.immutable

object model {

  abstract class HandrailError(reason: String) extends RuntimeException(reason)
  case class HandrailParseError(reason: String) extends HandrailError(reason)
  case class HandrailExecutionError(reason: String) extends HandrailError(reason)

  sealed trait Expression {
    def eval(value: Expression.Value): Expression.Value = this match {
      case v: Expression.Value => v
      case fn: Expression.Function => fn(value)
    }
  }

  object Expression {
    sealed trait Value extends Expression
    object Value {
      // TODO Add safestring
      case class String(value: scala.Predef.String) extends Value
      case class Number(value: scala.Double) extends Value
      case class Boolean(value: scala.Boolean) extends Value
      case class Array(values: Iterable[Expression]) extends Value
      case class Object(value: Map[scala.Predef.String, Expression.Value]) extends Value
      case object Void extends Value
    }
    trait Function extends Expression {
      def apply(value: Expression.Value): Expression.Value
    }
  }

  trait Template {
    def apply(data: Expression.Value): Either[HandrailExecutionError, String]
  }

  trait Helper {
    def apply(positionalArgs: List[Expression], nominalArgs: Map[String, Expression]): Expression.Function
  }

  // TODO Do not model in AST
  sealed trait EatSpace {
    def combine(other: EatSpace) = (this, other) match {
      case (x, y) if x == y => x
      case (x, EatSpace.None) => x
      case (EatSpace.None, x) => x
      case (_, EatSpace.Both) => EatSpace.Both
      case (EatSpace.Both, _) => EatSpace.Both
      case (EatSpace.Left, EatSpace.Right) => EatSpace.Both
      case (EatSpace.Right, EatSpace.Left) => EatSpace.Both
    }
  }

  // TODO Do not model in AST
  object EatSpace {
    case object Left extends EatSpace
    case object Right extends EatSpace
    case object Both extends EatSpace
    case object None extends EatSpace

    val left: EatSpace = EatSpace.Left
    val right: EatSpace = EatSpace.Right
    val both: EatSpace = EatSpace.Both
    val none: EatSpace = EatSpace.None
  }
}
