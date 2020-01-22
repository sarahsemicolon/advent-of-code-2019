import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class LayeredImageProcessorTest extends FunSuite with Matchers {
  test(s"Check should match expected output for test case") {
    new LayeredImageProcessor().doCheck(imageData = "123456789012", size = (3, 2)) should be (1)
  }

  test(s"Decode should match expected output for test case") {
    new LayeredImageProcessor().decodeImage(imageData = "0222112222120000", size = (2, 2)) should be (
      """01
        |10""".stripMargin)
  }

  test(s"Check should match expected output for input case") {
    new LayeredImageProcessor().doCheck(imageData = Source.fromResource("day8input.txt").getLines().next(), size = (25, 6)) should be (2413)
  }

  test(s"Decode should match expected output for input case") {
    new LayeredImageProcessor().decodeImage(imageData = Source.fromResource("day8input.txt").getLines().next(), size = (25, 6)) should be (
      """1110001100111001111011100
        |1001010010100100001010010
        |1110010000100100010011100
        |1001010000111000100010010
        |1001010010100001000010010
        |1110001100100001111011100""".stripMargin)
  }
}
