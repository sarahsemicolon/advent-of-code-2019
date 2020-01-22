package intcode

import scala.collection.mutable.Queue
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

object Computer {
  val immediateMode = '1'

  val opCodeInstructionLength = Map(
    1 -> 4,
    2 -> 4,
    3 -> 2,
    4 -> 2,
    5 -> 3,
    6 -> 3,
    7 -> 4,
    8 -> 4)

  var software = Seq[Int]()
  var instruction = ""
  var position = 0

  def getParam(paramPosition: Int, forceImmediateMode: Boolean = false) = {
    val mode = instruction(instruction.length - (2 + paramPosition))
    if (forceImmediateMode || (mode equals immediateMode)) software(position + paramPosition) else software(software(position + paramPosition))
  }

  def runProgram (softwareParam : Seq[Int], input : Queue[Int] = Queue[Int]()) : (Seq[Int], Seq[Int]) = {
    software = softwareParam
    position = 0
    var output = new ListBuffer[Int]()
    breakable { while (position < software.length) {
      if (software(position) equals 99) break
      breakable {
        instruction = software(position).toString
        val opCode = instruction.takeRight(2).toInt
        instruction = instruction.reverse.padTo(opCodeInstructionLength(opCode) + 1, "0").reverse.mkString

        opCode match {
          case 1 =>
            software = software.updated(getParam(3, true), getParam(1) + getParam(2))
            position += opCodeInstructionLength(opCode)
          case 2 =>
            software = software.updated(getParam(3, true), getParam(1) * getParam(2))
            position += opCodeInstructionLength(opCode)
          case 3 =>
            // always use immediate mode
            software = software.updated(getParam(1, true), input.dequeue)
            position += opCodeInstructionLength(opCode)
            break
          case 4 =>
            output += getParam(1)
            position += opCodeInstructionLength(opCode)
            break
          case 5 =>
            if (!(getParam(1) equals 0)) {
              position = getParam(2)
            } else {
              position += opCodeInstructionLength(opCode)
            }
            break
          case 6 =>
            if (getParam(1) equals 0) {
              position = getParam(2)
            } else {
              position += opCodeInstructionLength(opCode)
            }
          case 7 =>
            software = software.updated(getParam(3, true),
              if (getParam(1) < getParam(2)) 1 else 0)
            position += opCodeInstructionLength(opCode)
            break
          case 8 =>
            software = software.updated(getParam(3, true),
              if (getParam(1).equals(getParam(2))) 1 else 0)
            position += opCodeInstructionLength(opCode)
            break
        }
      }
    }}
    (software, output.toSeq)
  }

  def runProgramWithAdjustment(input: Seq[Int], noun: Int, verb: Int) : (Seq[Int], Seq[Int]) =
  {
    var program = input.updated(1, noun)
    program = program.updated(2, verb)
    Computer.runProgram(program)
  }
}
