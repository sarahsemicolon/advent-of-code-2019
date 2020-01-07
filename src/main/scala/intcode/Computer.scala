package intcode

import scala.util.control.Breaks.{break, breakable}

object Computer {
  var opCodeInstructionLength: Map[Int, Int] = Map(
    1 -> 4,
    2 -> 4)

  def runProgram (input : Seq[Int], inputInstruction : Int = 0) : (Seq[Int], Seq[Int]) = {
    var code = input
    var position = 0
    var output = Seq()
    breakable { while (position < code.length) {
      if (code(position) == 99) break

      var instruction = code(position).toString
      val opCode = instruction.takeRight(2).toInt
      instruction = instruction.reverse.padTo(opCodeInstructionLength(opCode), "0").reverse.mkString

      val mode1 = instruction(instruction.length - 2)
      val mode2 = instruction(instruction.length - 3)

      val param1 = if (mode1 == 1) code(position + 1) else code(code(position + 1))
      val param2 = if (mode2 == 1) code(position + 2) else code(code(position + 2))
      val indexToEdit = code(position + 3)

      opCode match {
        case 1 => code = code.updated(indexToEdit, param1 + param2)
        case 2 => code = code.updated(indexToEdit, param1 * param2)
      }

      position += opCodeInstructionLength(opCode)
    }}
    (code, output)
  }

  def runProgramWithAdjustment(input: Seq[Int], noun: Int, verb: Int) : (Seq[Int], Seq[Int]) =
  {
    var program = input.updated(1, noun)
    program = program.updated(2, verb)
    Computer.runProgram(program)
  }
}
