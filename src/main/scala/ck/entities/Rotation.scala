package ck
package entities

sealed trait Rotation {
  def value: Int
  def rotated: Rotation
  def reverseRotated: Rotation
}

object Rotation {
  def apply(value: Int): Option[Rotation] =
    value match {
      case 0 => Some(Zero)
      case 1 => Some(Once)
      case 2 => Some(Twice)
      case 3 => Some(Triple)
      case _ => None
    }

  case object Zero extends Rotation {
    val value: Int               = 0
    val rotated: Rotation        = Once
    val reverseRotated: Rotation = Triple
  }

  case object Once extends Rotation {
    val value: Int               = 1
    val rotated: Rotation        = Twice
    val reverseRotated: Rotation = Zero
  }

  case object Twice extends Rotation {
    val value: Int               = 2
    val rotated: Rotation        = Triple
    val reverseRotated: Rotation = Once
  }

  case object Triple extends Rotation {
    val value: Int               = 3
    val rotated: Rotation        = Zero
    val reverseRotated: Rotation = Twice
  }
}
