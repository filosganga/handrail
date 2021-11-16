package handrail

import scala.collection.immutable

object ast {

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

  sealed trait Ast

  case class Comment(
      content: String,
      escaped: Boolean = true,
      eatSpace: EatSpace = EatSpace.None
  ) extends Ast

  case class Text(value: String) extends Ast

  // TODO it should contain an expression
  case class Ref(
      name: String,
      escaped: Boolean = true,
      eatSpace: EatSpace = EatSpace.None
  ) extends Ast

  // TODO it should contain an helper
  case class Block(
      name: String,
      eatSpace: EatSpace = EatSpace.None,
      children: List[Ast]
  ) extends Ast

  sealed trait Expression
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
    case class Function(
        name: String,
        positionalArguments: List[Expression] = List.empty,
        namedArguments: Map[String, Expression] = Map.empty
    ) extends Expression
  }
}
