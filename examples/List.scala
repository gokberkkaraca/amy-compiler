object List extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def myFuncMatches(x: Int): Int = {
    val number: Int = 5;
    number match {
      case 7 => 17
      case 5 => 15
      case 3 => 13
      case 1 => 11
    }
  }
}
