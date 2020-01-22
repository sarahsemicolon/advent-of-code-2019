package intcode

import scala.collection.mutable

class AmplifierSeries(val phaseSettingSeq: Seq[Int], val software: Seq[Int]) {
  def runSoftware() : Int = {
    var secondInput = 0
    0 to 4 foreach (i => {
      secondInput = new Computer().runProgram(software, mutable.Queue(phaseSettingSeq(i), secondInput))._2.last
    })
    secondInput
  }
}
