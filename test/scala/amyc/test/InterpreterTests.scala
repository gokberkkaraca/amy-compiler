package amyc.test

import amyc.utils.Frontend
import amyc.interpreter.Interpreter
import org.junit.Test

class InterpreterTests extends TestSuite {
  val pipeline = Frontend andThen Interpreter

  val baseDir = "test/resources/interpreter"

  val outputExt = "txt"

  @Test def testFactorial = shouldOutput(List("Factorial", "Std"), "Factorial", "")

  @Test def testError = shouldFail("BasicError")
}
