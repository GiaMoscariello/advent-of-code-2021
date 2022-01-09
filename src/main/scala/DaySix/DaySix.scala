package DaySix

import DailyProblem.DailyProblems
import Utils.ReadInputFile
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, IOApp}
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}

/*

  Initial state: 3,4,3,1,2
  After  1 day:  2,3,2,0,1
  After  2 days: 1,2,1,6,0,8
  After  3 days: 0,1,0,5,6,7,8
  After  4 days: 6,0,6,4,5,6,7,8,8
  After  5 days: 5,6,5,3,4,5,6,7,7,8
  After  6 days: 4,5,4,2,3,4,5,6,6,7
  After  7 days: 3,4,3,1,2,3,4,5,5,6
  After  8 days: 2,3,2,0,1,2,3,4,4,5
  After  9 days: 1,2,1,6,0,1,2,3,3,4,8
  After 10 days: 0,1,0,5,6,0,1,2,2,3,7,8
  After 11 days: 6,0,6,4,5,6,0,1,1,2,6,7,8,8,8
  After 12 days: 5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8
  After 13 days: 4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8
  After 14 days: 3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8
  After 15 days: 2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7
  After 16 days: 1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8
  After 17 days: 0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8
  After 18 days: 6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8
                 0,1,0,0,0,2,2,2,1,1,3,3,2,2,4,4,3,5,5,7,6,8

  Each day, a 0 becomes a 6 and adds a new 8 to the end of the list,
   while each other number decreases by 1 if it was present at the start of the day.
 */

object DaySix extends DailyProblems {
  val example = "src/main/scala/DaySix/example.txt"
  val input = "src/main/scala/DaySix/input.txt"
  type Herd = Map[Int, List[Int]]

  override def firstProblem(): Unit = {
    val days = Range(0, 256).toList
    val startingFish = inputFromFile(input)
      .foldLeft(Array.fill(9)(0L).toList)((list, v) => list.updated(v, list(v) + 1))
    
    val herdAtDay = days.foldLeft(startingFish){ case (list, _) =>
        val herd = list.drop(1) ++ list.take(1)
        herd.updated(6, herd(6) + herd(8))
    }
    println(herdAtDay.sum)
  }

  override def secondProblem(): Unit = ???

  def inputFromFile(filename: String): Seq[Int] = {
    Files[IO].readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(line => line.split(",").map(_.toInt).toList)
      .compile
      .foldMonoid
      .unsafeRunSync()
  }
}


