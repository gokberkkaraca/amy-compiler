package amyc.test

import java.security._
import scala.runtime.ScalaRunTime
import java.io._

class SandboxedTest {

  // For you, a sandboxed test just runs its argument
  def sandboxedTest[T](test: => T): T = {
    test
  }

  def sandboxedTestWithRedirectedIO[T](test: => T, input: String): String = {
    import scala.Console._
    val inputS  = new BufferedInputStream(new ByteArrayInputStream(input.getBytes("UTF-8")))
    val outputS = new ByteArrayOutputStream()
    withOut(outputS) {
      withErr(outputS) {
        withIn(inputS) {
          sandboxedTest(test)
        }
      }
    }
    outputS.toString()
  }
}
