import scala.io.Source._

object DayOne {
  def simpleFuelCalculation(mass: Double) : Double = {
    return Math.floor(mass / 3) - 2
  }

  def complexFuelCalculation(mass: Double) : Double = {
    val fuelRequired = simpleFuelCalculation(mass)

    if (fuelRequired < 0) {
      return 0
    } else {
      return fuelRequired + complexFuelCalculation(fuelRequired)
    }
  }

  def fuelRequired(masses: Iterator[String], fuelFunction: (Double) => Double) : Double = {
    return masses
      .map(x => x.toDouble)
      .map(x => fuelFunction(x))
      .sum
  }
}
