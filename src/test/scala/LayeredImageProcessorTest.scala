import org.scalatest.{FunSuite, Matchers}

import scala.io.Source

class LayeredImageProcessorTest extends FunSuite with Matchers {
  test(s"Should match expected output for test case") {
    new LayeredImageProcessor().doCheck(imageData = "123456789012", size = (3, 2)) should be (1)
  }

  test(s"Should match expected output for input case") {
    new LayeredImageProcessor().doCheck(imageData = Source.fromResource("day8input.txt").getLines().next(), size = (25, 6)) should be (2413)
  }
}
