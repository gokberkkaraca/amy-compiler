package amyc.test

import amyc.utils._
import java.io.File
import java.security.PrivilegedActionException
import org.junit.Assert.fail

class CompilerTest extends SandboxedTest {
  private def runPipeline(pipeline: Pipeline[List[File], Unit], fileNames: List[String]) = {
    val ctx = Context(new Reporter, fileNames)
    val files = ctx.files.map(new File(_))
    pipeline.run(ctx)(files)
    ctx.reporter.terminateIfErrors()
  }

  private def runPipelineRedirected(
    pipeline: Pipeline[List[File], Unit],
    compiledFiles: List[String],
    input: String
  ): String = {
    sandboxedTestWithRedirectedIO(runPipeline(pipeline, compiledFiles), input)
  }

  private def assertEqual(output: String, expected: String) = {
    val rejectLine = (s: String) =>
      s.isEmpty ||
        s.startsWith("[ Info  ]") ||
        s.startsWith("[Warning]") ||
        s.startsWith("[ Error ]") ||
        s.startsWith("[ Fatal ]")
    def filtered(s: String) = s.lines.filterNot(rejectLine).mkString("\n")
    val filteredOutput = filtered(output)
    val filteredExpected = filtered(expected)
    if (filteredOutput != filteredExpected) {
      val sb = new StringBuffer()
      sb.append("\nOutput is different:\n")
      sb.append("\nOutput: \n")
      sb.append(filteredOutput)
      sb.append("\n\nExpected output: \n")
      sb.append(filteredExpected)
      fail(sb.toString)
    }
  }

  protected def compareOutputs(
    pipeline: Pipeline[List[File], Unit],
    compiledFiles: List[String],
    expectedFile: String,
    input: String = ""
  ) = {
    try {
      val output = runPipelineRedirected(pipeline, compiledFiles, input)
      val expected = scala.io.Source.fromFile(new File(expectedFile)).mkString
      assertEqual(output, expected)
    } catch {
      case e: PrivilegedActionException =>
        e.getException match {
          case AmycFatalError(msg) =>
            fail(s"\n  $msg\n")
          case other =>
            throw other
        }
    }
  }

  protected def demandFailure(
    pipeline: Pipeline[List[File], Unit],
    compiledFiles: List[String],
    input: String = ""
  ) = {
    try {
      runPipelineRedirected(pipeline, compiledFiles, input)
      fail("Test should fail but it passed!")
    } catch {
      case e: PrivilegedActionException =>
        e.getException match {
          case AmycFatalError(msg) =>
            // Ok, this is what we wanted
          case other =>
            // An error of an another kind should not be happening
            throw other
        }
    }

  }


}
