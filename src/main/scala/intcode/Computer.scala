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

  var code = Seq[Int]()
  var instruction = ""
  var position = 0

  def getParam(paramPosition: Int, forceImmediateMode: Boolean = false) = {
    val mode = instruction(instruction.length - (2 + paramPosition))
    if (forceImmediateMode || (mode equals immediateMode)) code(position + paramPosition) else code(code(position + paramPosition))
  }

  def runProgram (input : Seq[Int], inputInstruction : Int = 0) : (Seq[Int], Seq[Int]) = {
    code = input
    position = 0
    var output = new ListBuffer[Int]()
    breakable { while (position < code.length) {
      if (code(position) equals 99) break
      breakable {
        instruction = code(position).toString
        val opCode = instruction.takeRight(2).toInt
        instruction = instruction.reverse.padTo(opCodeInstructionLength(opCode) + 1, "0").reverse.mkString

        val param1 = getParam(1)

        opCode match {
          case 3 =>
            // always use immediate mode
            code = code.updated(getParam(1, true), inputInstruction)
            position += opCodeInstructionLength(opCode)
            break
          case 4 =>
            output += param1
            position += opCodeInstructionLength(opCode)
            break
          case _ =>
        }

        val param2 = getParam(2)
        val param3 = getParam(3, true)

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
