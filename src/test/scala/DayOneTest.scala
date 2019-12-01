import org.scalatest._
import Matchers._
import org.scalatest.FunSuite

class DayOneTest extends FunSuite with Matchers {
  case class Case(input: Int, expected: Int)

  private val simpleFuelCalculationCases = Seq(
    Case(12, 2),
    Case(14, 2),
    Case(1969, 654),
    Case(100756, 33583)
  )

  for (Case(input, expected) <- simpleFuelCalculationCases) {
    test(s"should match expected for simple fuel calculation - value $input") {
      DayOne.simpleFuelCalculation(input) should equal (expected)
    }
  }

  private val complexFuelCalculationCases = Seq(
    Case(14, 2),
    Case(1969, 966),
    Case(100756, 50346)
  )

  for (Case(input, expected) <- complexFuelCalculationCases) {
    test(s"should match expected for complex fuel calculation - value $input") {
      DayOne.complexFuelCalculation(input) should equal (expected)
    }
  }

  test("stringTestLoadingShouldWork") {
    val input = Iterator("12", "14")
    DayOne.fuelRequired(input, DayOne.simpleFuelCalculation) should be (4)
  }
}
