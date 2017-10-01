object MatchTest extends App {
  def intTest(a: Int): Unit = {
    a match {
      case x => Std.printInt(x+3)
    }
  }

  def listTest(l: L.List): Unit = {
    l match {
      case L.Nil() => Std.printString("Empty List")
      case L.Cons(h, t) => Std.printInt(h * 2)
    }
  }

  def stringTest(str: String): Unit = {
    str match {
      case str => Std.printString("found")
      case "a" => Std.printString("a found")
      case "b" => Std.printString("b found")
      case _ => Std.printString("invalid string input")
    }
  }

  val a = "a";

  intTest(10);
  stringTest("a");
  stringTest("b");
  stringTest("c");
  listTest(L.Cons(3, L.Nil()))
}
