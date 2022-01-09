
import DayFive.{DayFive, Point}
import org.scalatest.funsuite.AnyFunSuite

class DayFiveTest extends AnyFunSuite {

  test("split two points") {
    val input = List("0,9 -> 5,9", "8,0 -> 0,8")

    val output = input.flatMap { line =>
      takeTwoPoints(line)
        .map { point =>
          val cordinate = point.split(",")
          Point(cordinate(0).toInt, cordinate(1).toInt)
        }
    }.sliding(2,2).toList

    println(output)
  }

  private def takeTwoPoints(line: String) = {
    line.stripMargin.split("->").toList.map(_.trim)
  }

  test("first problem") {
    DayFive.firstProblem()
  }

  test("grid tests") {
    val grid = Array.ofDim[Int](9,9)
    grid.map(_ =>  Array.emptyIntArray)
    grid.update(4, grid(4).take(5) ++ Array.fill(4)(4))
    println(grid.foreach(a => println(a.mkString)))
  }
}
