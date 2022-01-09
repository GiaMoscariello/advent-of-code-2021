import DaySeven.DaySeven
import org.scalatest.funsuite.AnyFunSuite

class DaySeven extends AnyFunSuite{

  test("fuel calculate") {
    val result = DaySeven.firstProblem()
    println(result)
  }

  test("second problem") {
    println(DaySeven.secondProblem())
  }
}
