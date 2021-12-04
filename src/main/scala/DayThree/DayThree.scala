package DayThree

import Utils.ReadInputFile

object DayThree {
  val input: List[String] = ReadInputFile.toStrings("src/main/scala/DayThree/input.txt")

  def firstProblem(): Integer = {
    calculate(input)
  }

  private def calculate(list: List[String]):  Integer = {
    val indexs = list.find(s => s.nonEmpty).map(value => value.length)
    if (indexs.isDefined) {
      val (gamma, epsilon) = loop(list, indexs.get - 1, ("", ""))
      Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    } else 0
  }

  private def loop(list: List[String], index: Integer, rates: (String, String)): (String, String) = {
    val (gammas, epsilon) = rates
    list match {
      case _ :: _ if (index >= 0) =>
        val (most, less) = calculateRates(list, index);
        loop(list, index -1, (most.toString.concat(gammas), less.toString.concat(epsilon)))
      case _ => rates
    }
  }

  private def calculateRates(list: List[String], index: Integer): (Char, Char) = {
    val l = list.map(value => value.charAt(index))
    val (zeros, ones) = l.partition(c => c == '0')
    if (zeros.length > ones.length) (zeros.head, ones.head) else (ones.head, zeros.head)
  }
}
