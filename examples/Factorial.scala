object Factorial extends App {
  def fact(i: Int): Int = {
    if (i < 2) { 1 }
    else { i * fact(i-1) }
  }

  Std.printString("5! = "  ++ Std.intToString(fact(5)));
  Std.printString("10! = " ++ Std.intToString(fact(10)))
}
