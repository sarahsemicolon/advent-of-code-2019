package intcode

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Computer {
  val immediateMode = '1'

  val opCodeInstructionLength = Map(
    1 -> 4,
    2 -> 4,
    3 -> 2,
    4 -> 2)

  def runProgram (input : Seq[Int], inputInstruction : Int = 0) : (Seq[Int], Seq[Int]) = {
    var code = input
    var position = 0
    var output = new ListBuffer[Int]()
    breakable { while (position < code.length) {
      if (code(position) equals 99) break
      breakable {
        var instruction = code(position).toString
        val opCode = instruction.takeRight(2).toInt
        instruction = instruction.reverse.padTo(opCodeInstructionLength(opCode) + 1, "0").reverse.mkString

        val mode1 = instruction(instruction.length - 3)
        val param1 = if (mode1 equals immediateMode) code(position + 1) else code(code(position + 1))

        opCode match {
          case 3 =>
            // always use immediate mode
            code = code.updated(code(position + 1), inputInstruction)
            position += opCodeInstructionLength(opCode)
            break
          case 4 =>
            output += param1
            position += opCodeInstructionLength(opCode)
            break
          case _ =>
        }

        val mode2 = instruction(instruction.length - 4)
        val param2 = if (mode2 equals immediateMode) code(position + 2) else code(code(position + 2))

        //Always use immediate mode for param3 at the moment
        val param3 = code(position + 3)

        opCode match {
          case 1 =>
            code = code.updated(param3, param1 + param2)
            position += opCodeInstructionLength(opCode)
          case 2 =>
            code = code.updated(param3, param1 * param2)
            position += opCodeInstructionLength(opCode)
        }
      }
    }}
    (code, output.toSeq)
  }

  def runProgramWithAdjustment(input: Seq[Int], noun: Int, verb: Int) : (Seq[Int], Seq[Int]) =
  {
    var program = input.updated(1, noun)
    program = program.updated(2, verb)
    Computer.runProgram(program)
  }
}
