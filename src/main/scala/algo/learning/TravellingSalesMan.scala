package algo.learning

object TravellingSalesMan extends App {
  val distanceMatrix = List(List(0, 10, 15, 20), List(10, 0, 35, 25), List(15, 35, 0, 30), List(20, 25, 30, 0))

  def d(i: Int, j: Int) = distanceMatrix(i)(j)

  def c(s: Set[Int], i: Int): (String, Int) = {
    if (s.size == 2)
      (i.toString, d(0, i))
    else if (s.size > 2) {
      (s diff Set(0, i)).map(j => {
        val subPathCost = c(s diff Set(i), j)
        (subPathCost._1 + " -> " + i, subPathCost._2 + d(j, i))
      }).minBy(_._2)
    }
    else
      (i.toString, 0)
  }

  val cities = Set(0, 1, 2, 3)
  val (path, cost) = (cities diff Set(0)).map(i => {
    val cost = c(cities, i)
    (0 + " -> " + cost._1 + " -> " + 0, +cost._2 + d(i, 0))
  }).minBy(_._2)

  println(s"Minimum Cost: $cost")
  println(s"Respective Path: $path")
}
