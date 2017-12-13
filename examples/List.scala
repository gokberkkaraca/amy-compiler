object List extends App {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List

  def myFuncMatches(x: Int): Int = {
    val str: String = "gokberk";
    str match {
      case "gokberk" => 5
    }
  }
}
