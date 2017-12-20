package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "test/resources/lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")

  @Test def testSingleAmp = shouldFail("SingleAmp")
}
