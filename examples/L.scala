object L extends App {

  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  abstract class LPair
  case class LP(l1: List, l2: List) extends LPair

  def split(l: List): LPair = {
    l match {
      case Cons(h1, Cons(h2, t)) =>
        val rec: LPair = split(t);
        rec match {
          case LP(rec1, rec2) =>
            LP(Cons(h1, rec1), Cons(h2, rec2))
        }
      case _ =>
        LP(l, Nil())
    }
  }
}