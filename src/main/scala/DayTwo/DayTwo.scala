package DayTwo

import Utils.ReadInputFile

import scala.annotation.tailrec

object DayTwo {
  val input: List[(String, Integer)] = toPair("src/main/scala/DayTwo/input.txt")

  def firstProblem(): Integer = {
    calculate(input)
  }

  def secondProblem(): Integer = {
    calculateEnhanced(input)
  }

  private def calculate(lst: List[(String, Integer)]): Integer = {
    @tailrec
    def loop(lst: List[(String, Integer)], acc: (Integer, Integer)): (Integer, Integer) = lst match {
      case head :: tail if head._1 == "forward"   => val (_, value) = head; loop(tail, (acc._1 + value, acc._2))
      case head :: tail if head._1 == "up"        => val (_, value) = head; loop(tail, (acc._1, acc._2 - value))
      case head :: tail if head._1 == "down"      => val (_, value) = head; loop(tail, (acc._1, acc._2 + value))
      case _                            => acc
    }
    val pos = loop(lst, (0, 0))
    pos._1 * pos._2
  }

  private def calculateEnhanced(lst: List[(String, Integer)]): Integer = {
    @tailrec
    def loop(lst: List[(String, Integer)], position: (Integer, Integer, Integer)): (Integer, Integer, Integer) = {
      val (hor, depth, aim) = position
      lst match {
        case head :: tail if head._1 == "forward"   => val (_ , value) = head; loop(tail, (hor + value, depth + (value * aim), aim))
        case head :: tail if head._1 == "up"        => val (_ , value) = head; loop(tail, (hor, depth , aim - value))
        case head :: tail if head._1 == "down"      => val (_ , value) = head; loop(tail, (hor , depth, aim + value))
        case _                                      => position
      }
    }
    val (hor, depth, _) = loop(lst, (0, 0, 0))
    hor * depth
  }

  private def toPair(filename: String): List[(String, Integer)] = {
    val source = scala.io.Source.fromFile(filename)
    try source.getLines().toList.map{
      line =>
        val splitLine: Array[String] = line.split(" ")
        (splitLine(0), splitLine(1).toInt)
    } finally source.close()
  }
}
