package DayFive

import DailyProblem.DailyProblems
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp, Resource, Sync}

import scala.concurrent.ExecutionContext
import scala.io.BufferedSource
import scala.io.Source.fromFile

object DayFive extends DailyProblems {
  val example = "src/main/scala/DayFive/example.txt"
  val input = "src/main/scala/DayFive/input.txt"


  override def firstProblem(): Unit = {
    val value = for {
      setup   <- inputToVentSetup(example)
      grid    = buildDiagram(setup)
      res     = grid.map.flatMap((line: Array[Int]) => line.filter(x => x >= 2)).length
    } yield res
    value.map(v => println(v)).unsafeRunSync()
  }

  override def secondProblem(): Unit = ???

  def inputToVentSetup(filename: String): IO[LinesVent] = {
    file(filename).use {
      file =>
        IO {
          val list = file
            .getLines()
            .toList
            .flatMap(line => takeTwoPoints(line).map (points => Point(points)))
            .sliding(2, 2)
            .toList
            .map(pair => (pair(0), pair(1)))
          LinesVent(list)
        }
    }
  }

  private def takeTwoPoints(line: String) =
    line.stripMargin.split("->").toList.map(_.trim)

   def file[F[_]](name: String)(implicit F: Sync[IO]): Resource[IO, BufferedSource] =
    Resource.make(IO(fromFile(name)))(file => IO(file.close()))

  private def maxDim(vents: List[(Point, Point)]): (Int, Int) = vents
    .map(p => ((p._1.x, p._1.y), (p._2.x, p._2.y)))
    .foldLeft((0,0)) {
      case ((accX, accY), ((x1, y1), (x2, y2)))=>
        val x = Seq(accX, x1, y2).max
        val y = Seq(accY, x2, y2).max
        (x, y)
    }

  private def buildDiagram(vent: LinesVent): Grid = {
    val (x, y) = maxDim(vent.lines)
    val grid = Grid(x, y)
    val vertical = vent.lines.filter{ line => val (p1, p2) = line; p1.x == p2.x }
    val horizontal = vent.lines.filter{ line => val (p1, p2) = line; p1.y == p2.y }
    val diagonal = vent.lines.filter {line => val (p1, p2) = line; p1.x != p2.x && p2.y != p1.y}
    println(diagonal)
    vertical.foreach { points: (Point, Point) =>
      val (p1, p2) = if (points._1.y < points._2.y) points else points.swap
      val range = p1.y.to(p2.y, 1)
      range.foreach(i => grid.map(i)(p1.x) += 1)
    }
    horizontal.foreach { points: (Point, Point) =>
      val (p1, p2) = if (points._1.x < points._2.x) points else points.swap
      val range = p1.x.to(p2.x, 1)
      range.foreach(i => grid.map(p1.y)(i) += 1)
    }
//    diagonal.foreach { points: (Point, Point) =>
//      val (p1, p2) = if (points._1.x < points._2.x) points else points.swap
//      val range = p1.x.to(p2.x, 1)
//      range.foreach(i => grid.map(p1.y)(i) += 1)
//    }
    grid.map.foreach(l => println(l.mkString))
    grid
  }
}

case class LinesVent(lines: List[(Point, Point)])

case class Grid(map: Array[Array[Int]] = Array.ofDim(10, 10), rank: Int = 0)

case class Point(x: Int, y: Int)

object Point {
  def apply(points: String): Point = {
    val cordinate = points.split(",")
    Point(cordinate(0).toInt, cordinate(1).toInt)
  }
}

object Grid {
  def apply(dimX: Int, dimY: Int): Grid = {
    val grid = Array.ofDim[Int](dimX + 1, dimY + 1)
    grid.map(_ => Array.emptyIntArray)
    Grid(grid)
  }
}