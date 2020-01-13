object OrbitCounter {
  def countOrbitsInMap(input: String) : Int = {
    val orbitMap = splitInputToMap(input)
    orbitMap.keys.map(x => (x, countOrbits(x, orbitMap))).toMap.values.sum
  }

  private def splitInputToMap(input: String) : Map[String, String] = {
    input.split('\n').map(row => (row.split(')')(1), row.split(')')(0))).toMap
  }

  private def countOrbits(node: String, orbitMap: Map[String, String]) : Int = {
    if (orbitMap contains node) 1 + countOrbits(orbitMap(node), orbitMap) else 0
  }
}
