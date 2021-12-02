package DayTwo

import Utils.ReadInputFile

import scala.annotation.tailrec

object DayTwo {
  val input: List[(String, Integer)] = ReadInputFile.toPair("src/main/scala/DayTwo/input.txt")
  def firstProblem(): Integer = {
    calculate(input)
  }

  private def calculate(lst: List[(String, Integer)]): Integer = {
    @tailrec
    def loop(lst: List[(String, Integer)], acc: (Integer, Integer)): (Integer, Integer) = lst match {
      case Nil                          => acc
      case h :: t if h._1 == "forward"  => loop(t, (acc._1 + h._2, acc._2))
      case h :: t if h._1 == "up"       => loop(t, (acc._1, acc._2 - h._2))
      case h :: t if h._1 == "down"     => loop(t, (acc._1, acc._2 + h._2))
      case _                            => acc
    }
    val pos = loop(lst, (0, 0))
    pos._1 * pos._2
  }
}
