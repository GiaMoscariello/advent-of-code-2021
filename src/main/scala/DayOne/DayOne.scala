package DayOne

import scala.annotation.tailrec

object DayOne {
  val input: List[Integer] = ReadInput.readString("src/main/scala/DayOne/input.txt")
  def firstProblem(): Integer = {
    calculate(input)
  }

  def secondProblem(): Integer = {
    val inputSlidingWindow = calculateWindow(input)
    calculate(inputSlidingWindow)
  }

  private def calculate(lst: List[Integer]): Integer = {
    @tailrec
    def loop(lst: List[Integer], acc: Int, prev: Integer): Integer = lst match {
      case Nil                  => acc
      case h :: t if (h > prev) => loop(t, acc + 1, h)
      case h :: t               => loop(t, acc, h)
    }
    loop(lst, -1, 0)
  }

  private def calculateWindow(lst: List[Integer]) : List[Integer] = {
    @tailrec
    def loop(list:List[Integer], acc: List[Integer]): List[Integer] = list match {
      case Nil                        => acc
      case h :: n :: t if t.nonEmpty  => loop(n :: t, acc :+ (h + n + t.head))
      case _ :: t  if t.nonEmpty      => loop(List(), acc)
    }
    loop(lst, List())
  }
}

object ReadInput {
  def readString(filename: String): List[Integer] = {
    val source = scala.io.Source.fromFile(filename)
    try source.getLines().toList.map(_.toInt) finally source.close()
  }
}