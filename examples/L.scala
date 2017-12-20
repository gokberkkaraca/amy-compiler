object L extends App {

  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  abstract class NumPair
  case class Three(x: Int, y: Int, z: Int) extends NumPair
  case class Four(x: Int, y: Int, z: Int, t: Int) extends NumPair

  abstract class LPair
  case class LP(l1: List, l2: List) extends LPair

  def contains(l: List, elem: Int): Boolean = { l match {
    case Nil() =>
      false
    case Cons(h, t) =>
      h == elem || contains(t, elem)
  }}

  def threeTest(): Boolean = {
    Three(3, 5, 7) match {
      case Three(2, 3, 5) => false
    }
  }

  def fourtest(): Boolean = {
    Four(3, 5, 7, 9) match {
      case Four(2, 4, 6, 8) => false
    }
  }
}