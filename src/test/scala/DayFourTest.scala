import org.scalatest._
import org.scalatest.FunSuite

class DayFourTest extends FunSuite with Matchers {
  private case class Case (input: String, expected: Boolean)

  private val part1passwordCases = Seq(
    Case("111111", true),
    Case("223450", false),
    Case("123789", false)
  )

  private val part2passwordCases = Seq(
    Case("112233", true),
    Case("123444", false),
    Case("111122", true)
  )

  for (Case(input, expected) <- part1passwordCases) {
    test(s"should match expected for part 1 password cases - value $input") {
      DayFour.isPotentialPassword(input) should be (expected)
    }
  }

  for (Case(input, expected) <- part2passwordCases) {
    test(s"should match expected for part 2 password cases - value $input") {
      DayFour.isPotentialPassword(input, true) should be (expected)
    }
  }

  val input = 138241 to 674034
  println(input.map(x => DayFour.isPotentialPassword(x.toString)).count(x => x))
  println(input.map(x => DayFour.isPotentialPassword(x.toString, true)).count(x => x))
}
