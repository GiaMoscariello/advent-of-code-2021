package Utils

import DayFour.{Bingo, Table}

object ReadInputFile {

  def toList(filename: String): List[Integer] = {
    val source = scala.io.Source.fromFile(filename)
    try source.getLines().toList.map(_.toInt) finally source.close()
  }

  def toStrings(filename: String): List[String] = {
    val source = scala.io.Source.fromFile(filename)
    try source.getLines().toList finally source.close()
  }

  def toPair(filename: String): List[(String, Integer)] = {
    val source = scala.io.Source.fromFile(filename)
    try source.getLines().toList.map{
      line =>
        val splitLine: Array[String] = line.split(" ")
        (splitLine(0), splitLine(1).toInt)
    } finally source.close()
  }

  def toBingoTable(filename: String): Bingo = {
    val source = scala.io.Source.fromFile(filename)
    try {
      val (nums, tables) = source.getLines().toList.filter(line => line.nonEmpty).splitAt(1)
      val drawnNums: List[String] = nums.head.split(",").toList
      val bingoTables: List[Table] = tables
        .sliding(5,5)
        .zipWithIndex
        .map { table: (List[String], Int) =>
          val (list, num) = table
          val bingoTable: Seq[List[Int]] = list.map(line => line
            .split(" ")
            .toList
            .filter(line => line.nonEmpty)
            .map(_.toInt))
          Table(bingoTable, num)
        }.toList

      Bingo(drawnNums, bingoTables)
    } finally source.close()
  }
}
