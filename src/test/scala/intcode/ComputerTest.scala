package intcode

import org.scalatest.{FunSuite, _}

import scala.util.control.Breaks._

class ComputerTest extends FunSuite with Matchers{
  case class Case(input: Seq[Int], expected: Seq[Int])

  private val programChangeCases = Seq(
    Case(Seq(1,0,0,0,99), Seq(2,0,0,0,99)),
    Case(Seq(2,3,0,3,99), Seq(2,3,0,6,99)),
    Case(Seq(2,4,4,5,99,0), Seq(2,4,4,5,99,9801)),
    Case(Seq(1,1,1,4,99,5,6,0,99), Seq(30,1,1,4,2,5,6,0,99)),
    Case(Seq(1002,4,3,4,33), Seq(1002, 4, 3, 4, 99)),
    Case(Seq(1101,100,-1,4,0), Seq(1101, 100, -1, 4, 99))
  )

  for (Case(input, expected) <- programChangeCases) {
    test(s"Program should be changed after execution as expected - $input becomes $expected") {
      val result = Computer.runProgram(input)
      result._1 should equal (expected)
      println(result._2)
    }
  }
  val input = Seq(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,9,23,1,23,13,27,1,10,27,31,2,31,13,35,1,10,35,39,2,9,39,43,2,43,9,47,1,6,47,51,1,10,51,55,2,55,13,59,1,59,10,63,2,63,13,67,2,67,9,71,1,6,71,75,2,75,9,79,1,79,5,83,2,83,13,87,1,9,87,91,1,13,91,95,1,2,95,99,1,99,6,0,99,2,14,0,0)

  test(s"With adjustment, should match expected for real program") {
    Computer.runProgramWithAdjustment(input, 12, 2)._1(0) should equal (3085697)
  }

  val range = 1 to 99
  val pairs = range.flatMap(x => range.map(y => (x,y)))

  breakable { for (n <- pairs) {
    if (Computer.runProgramWithAdjustment(input, n._1, n._2)._1(0) == 19690720) {
      n._1 should equal (94)
      n._2 should equal (25)
    }
  }}
}
