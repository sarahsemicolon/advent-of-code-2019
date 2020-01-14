import scala.collection.mutable.Queue

object OrbitCounter {
  def countOrbitsInMap(input: String) : Int = {
    val orbitMap = splitInputToMap(input)
    orbitMap.keys.map(x => (x, countOrbits(x, orbitMap))).toMap.values.sum
  }

  def countOrbitsTransfersInMap(input: String) : Int = {
    val orbitMap = splitInputToMap(input)
    val you = getObjectsToOrigin("YOU", orbitMap)
    val san = (getObjectsToOrigin("SAN", orbitMap))
    while (you(0).equals(san(0))) {
      you.dequeue()
      san.dequeue()
    }
    you.length + san.length - 2 // all the unique elements, minus YOU and SAN
  }

  private def splitInputToMap(input: String) : Map[String, String] = {
    input.split('\n').map(row => (row.split(')')(1), row.split(')')(0))).toMap
  }

  private def countOrbits(node: String, orbitMap: Map[String, String]) : Int = {
    if (orbitMap contains node) 1 + countOrbits(orbitMap(node), orbitMap) else 0
  }

  private def getObjectsToOrigin(node: String, orbitMap: Map[String, String]) : Queue[String] = {
    if (orbitMap contains node) getObjectsToOrigin(orbitMap(node), orbitMap) addOne node else new Queue
  }
}
