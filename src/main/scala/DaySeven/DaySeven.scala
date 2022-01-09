package DaySeven

import DailyProblem.DailyProblems
import Utils.ReadInputFile

object DaySeven extends DailyProblems {
  val example = "src/main/scala/DaySeven/example.txt"
  val input = "src/main/scala/DaySeven/input.txt"
  val crabs: Seq[Int] = ReadInputFile.inputToList(input)
  val positions: Seq[Int] = Range(1, crabs.size).toList

  // O(n^2)
  override def firstProblem(): Unit = println(fuelConsumed(positions, crabs)(distance1))
  // O(n^2)
  override def secondProblem(): Unit = println(fuelConsumed(positions, crabs)(distance2))

  private def fuelConsumed(pos: Seq[Int], crabs: Seq[Int])(distance: (Int, Int) => Int): Int =
    pos.map(x => calculate(x, crabs)(distance)).min

  private def calculate(x: Int, crabs: Seq[Int])(distance: (Int, Int) => Int): Int =
    crabs.foldLeft(0)((acc, y) => acc + distance(x, y))

  private val distance1 = (x: Int, y: Int) => Math.abs(x - y)
  private val distance2 = (x: Int, y: Int) => Range(0, Math.abs(x - y)).sum
}
