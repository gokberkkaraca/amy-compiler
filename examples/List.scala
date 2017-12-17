object List extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def myFuncMatches(x: List): Int = {
    Cons(33, Nil()) match {
      case Nil() => 0
      case Cons(h, t) => 3
    }
  }
}
