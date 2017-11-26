object FunctionTest extends App {

  def funcWithTwoParam(n: Int, str: String): Unit = {
    Std.printInt(n);
    Std.printString(str)
  }

  funcWithTwoParam(10, "Gokberk")
}