object ParamAndLocal extends App {
  def foo(i: Int): Int = {
    val i: Int = i + 1;
    i
  }
}
