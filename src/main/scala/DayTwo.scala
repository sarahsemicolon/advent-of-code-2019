import util.control.Breaks._

object DayTwo {
  def runProgram (input : Seq[Int]) : Seq[Int] = {
    var program = input
    var position = 0
    var stop = false

    breakable { while (position < program.length) {
      val opcode = program(position)

      if (opcode == 99) break

      val indexOfValue1 = program(position + 1)
      val indexOfValue2 = program(position + 2)
      val indexToEdit = program(position + 3)

      opcode match {
        case 1 => program = program.updated(indexToEdit, program(indexOfValue1) + program(indexOfValue2))
        case 2 => program = program.updated(indexToEdit, program(indexOfValue1) * program(indexOfValue2))
      }

      position += 4
    }}
    return program
  }

  def runProgramWithAdjustment(input: Seq[Int], noun: Int, verb: Int) : Seq[Int] =
  {
    var program = input.updated(1, noun)
    program = program.updated(2, verb)

    return DayTwo.runProgram(program)
  }
}
