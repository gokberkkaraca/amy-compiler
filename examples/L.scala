object L extends App {

  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  abstract class LPair
  case class LP(l1: List, l2: List) extends LPair

  def contains(l: List, elem: Int): Boolean = { l match {
    case Nil() =>
      false
    case Cons(h, t) =>
      h == elem || contains(t, elem)
  }}
}