object L extends App {

  abstract class List

  case class Nil() extends List

  case class Cons(h: Int, t: List) extends List

  def concat(l1: List, l2: List): List = {
    Cons(3 + 4, Nil()) match {
      case Nil() => l2
      case Cons(7, t) => Cons(5, Nil())
    }
  }
}