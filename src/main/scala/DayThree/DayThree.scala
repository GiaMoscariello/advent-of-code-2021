package DayThree

import Utils.ReadInputFile

import scala.annotation.tailrec

object DayThree {
  val input: List[String] = ReadInputFile.toStrings("src/main/scala/DayThree/input.txt")

  def firstProblem(): Integer = {
    calculate(input)
  }

  def secondProblem(): Integer = {
    calculateSecond(input)
  }

  private def calculate(list: List[String]): Integer = {
    val indexes = list.find(s => s.nonEmpty).map(value => value.length)
    if (indexes.isDefined) {
      val (gamma, epsilon) = loop(list, indexes.get - 1, ("", ""))
      Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    } else 0
  }

  @tailrec
  private def loop(list: List[String], index: Integer, rates: (String, String)): (String, String) = {
    val (gammas, epsilon) = rates
    list match {
      case _ :: _ if (index >= 0) =>
        val (most, less) = mostCommonBits(list, index);
        loop(list, index - 1, (most.toString.concat(gammas), less.toString.concat(epsilon)))
      case _ => rates
    }
  }

  private def mostCommonBits(list: List[String], index: Integer): (Char, Char) = {
    val l = list.map(value => value.charAt(index))
    val (zeros, ones) = l.partition(c => c == '0')
    if (zeros.length > ones.length) (zeros.head, ones.head) else (ones.head, zeros.head)
  }


  def calculateSecond(list: List[String]): Int = {
    val oxygen = oxygenRate(list, 0)
    val co2 = co2Rate(list, 0)
    Integer.parseInt(oxygen.head, 2) * Integer.parseInt(co2.head, 2)
  }

  @tailrec
  private def oxygenRate(oxygen: List[String], index: Integer): List[String] = oxygen match {
    case h :: _ if index < h.length && oxygen.length > 1 =>
      val (most, _) = mostCommonBits(oxygen, index)
      val o2Rate = oxygen.filter(v => v.charAt(index) == most)
      oxygenRate(o2Rate, index + 1)
    case _ => oxygen
  }

  @tailrec
  private def co2Rate(list: List[String], index: Integer): List[String] = list match {
    case h :: _ if index < h.length && list.length > 1 =>
      val (_, less) = mostCommonBits(list, index)
      val co2 = list.filter(v => v.charAt(index) == less)
      co2Rate(co2, index + 1)
    case _ => list
  }
}
