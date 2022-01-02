package DayFour

import Utils.ReadInputFile

import scala.annotation.tailrec


object DayFour {
  val game: Bingo = ReadInputFile.toBingoTable("src/main/scala/DayFour/input.txt")

  def firstProblem(): Unit = {
    val (winner, last) = findWinner(game.drawNumbers.map(_.toInt), game, Int.MinValue)
    if (winner.isDefined) {
      val numbers: Seq[Int] = winner.get.values.flatten
      println(numbers.filter(x => x != Int.MinValue).sum * last)
    }
  }

  def secondProblem(): Unit = {
    val (winner, last) = findLastWinner(game.drawNumbers.map(_.toInt), game, Int.MinValue)
    if (winner.isDefined) {
      val numbers: Seq[Int] = winner.get.values.flatten
      println(numbers.filter(x => x != Int.MinValue).sum * last)
    }
  }

  @tailrec
  def findWinner(drawNumbers: List[Int], bingo: Bingo, lastDrawn: Int): (Option[Table], Int) = {
    drawNumbers match {
      case head :: tail if bingo.BINGO().isEmpty =>
        findWinner(tail, bingo.numberExtraction(head), head)
      case _ => (bingo.BINGO(), lastDrawn)
    }
  }

  @tailrec
  def findLastWinner(drawNumbers: List[Int], bingo: Bingo, lastDrawn: Int): (Option[Table], Int) = {
    drawNumbers match {
      case head :: tail if (!bingo.hasOneTable)=>
        val game = bingo.extractNumberAndDeleteWinningTable(head)
        findLastWinner(tail, game, head)
      case head :: tail if (bingo.hasOneTable() && bingo.BINGO().isEmpty) =>
        val game = bingo.numberExtraction(head)
        findLastWinner(tail, game, head)
      case _ => (bingo.BINGO(), lastDrawn)
    }
  }
}
