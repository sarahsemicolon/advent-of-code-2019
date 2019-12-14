object DayThree {
  private case class Point(x: Int, y: Int)

  private val directionMap = Map(
    "L" -> Point(-1, 0),
    "R" -> Point(1, 0),
    "U" -> Point(0, 1),
    "D" -> Point(0, -1)
  )

  private def addPoints(point1: Point, point2: Point) : Point = Point(point1.x + point2.x, point1.y + point2.y)
  private def distanceFromOrigin(point: Point) : Int = Math.abs(point.x) + Math.abs(point.y)

  private def pointsVisited(path : String) : Seq[Point] = {
    var pointsVisited : Seq[Point] = Seq()
    var currentPoint = Point(0, 0)

    for (instruction <- path.split(",")) {
      for (i <- 1 to Integer.parseInt(instruction.substring(1))) {
        currentPoint = addPoints(currentPoint, directionMap(instruction.substring(0, 1)))
        pointsVisited = pointsVisited :+ currentPoint
      }
    }
    return pointsVisited
  }

  def findDistanceToClosestCrossing(path1 : String, path2 : String) : Int = {
    distanceFromOrigin(pointsVisited(path1) intersect pointsVisited(path2) minBy { point => distanceFromOrigin(point) })
  }
}
