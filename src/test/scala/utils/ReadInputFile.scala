package utils

import DayFour.{Bingo, DayFour}
import Utils.ReadInputFile
import org.scalatest.funsuite.AnyFunSuite

class ReadInputFile extends AnyFunSuite {


  test("toBingo") {
    val filename = "src/main/scala/DayFour/input.txt"
    val bingo = ReadInputFile.toBingoTable(filename)
    DayFour.secondProblem()
    }

}
