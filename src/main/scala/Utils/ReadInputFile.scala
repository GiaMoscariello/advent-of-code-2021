package Utils

import DayFour.{Bingo, Table}
import cats.data.NonEmptyList
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource, Sync}
import fs2.io.file.{Files, Path}
import fs2.text

import scala.io.BufferedSource
import scala.io.Source.fromFile

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
    val source: BufferedSource = scala.io.Source.fromFile(filename)
    try {
      val (nums, tables) = source.getLines().toList.filter(line => line.nonEmpty).splitAt(1)
      val drawnNums: List[String] = nums.head.split(",").toList
      val bingoTables: List[Table] = tables
        .sliding(5,5)
        .zipWithIndex
        .map { table: (List[String], Int) =>
          val (list, num) = table
          val bingoTable: Seq[List[Int]] = list
            .map(line => line
            .split(" ")
            .toList
            .filter(line => line.nonEmpty)
            .map(_.toInt))
          Table(bingoTable, num)
        }.toList

      Bingo(drawnNums, bingoTables)
    } finally source.close()
  }

  def inputToList(filename: String): Seq[Int] = {
    Files[IO].readAll(Path(filename))
      .through(text.utf8.decode)
      .through(text.lines)
      .map(line => line.split(",").map(_.toInt).toList)
      .compile
      .foldMonoid
      .unsafeRunSync()
  }

  def file[F[_]](name: String)(implicit F: Sync[IO]): Resource[IO, BufferedSource] =
    Resource.make(IO(fromFile(name)))(file => IO(file.close()))
}
