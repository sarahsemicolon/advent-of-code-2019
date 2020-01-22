import org.scalatest.{FunSuite, Matchers}

import scala.io.Source
import scala.reflect.internal.util.FileUtils

class LayerVerifierTest extends FunSuite with Matchers {
  test(s"Should match expected output for test case") {
    new LayerVerifier().doCheck(imageData = "123456789012", size = (3, 2)) should be (1)
  }

  test(s"Should match expected output for input case") {
    new LayerVerifier().doCheck(imageData = Source.fromResource("day8input.txt").getLines().next(), size = (25, 6)) should be (2413)
  }
}
