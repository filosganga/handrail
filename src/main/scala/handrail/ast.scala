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
      eatSpace: EatSpace = EatSpace.None
  ) extends Ast
}
