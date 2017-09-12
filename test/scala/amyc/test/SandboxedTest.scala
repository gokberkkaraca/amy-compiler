package amyc.test

import java.security._
import scala.runtime.ScalaRunTime
import java.io._

/**
  * Support for running tests in protected mode, so they don't trash our machines
  *
  * @author Lukas Rytz <lukas.rytz@epfl.ch>
  * @author Vlad Ureche <vlad.ureche@epfl.ch>
  */
class SandboxedTest {

  // force loading ScalaRunTime, which cannot load during the test due to
  // the security policy in place, that prevents reflection
  ScalaRunTime
  compat.Platform

  /** Run test in a restricted environment, where the code is not allowed to mess up the machine */
  def sandboxedTest[T](test: => T): T = {
    val action = new PrivilegedAction[T] {
      def run: T = { test }
    }
    val originalContext = AccessController.getContext
    val combiner = new DomainCombiner {
      def combine(p1: Array[ProtectionDomain], p2: Array[ProtectionDomain]): Array[ProtectionDomain] = {
        // revoke all permissions
        Array(new ProtectionDomain(null, new Permissions()))
      }
    }
    val cntext = new AccessControlContext(originalContext, combiner)
    AccessController.doPrivileged(action, cntext)
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
