import DaySix.DaySix
import cats.effect.IO
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite

class DaySixTest extends AnyFunSuite {
  val example = "src/main/scala/DaySix/example.txt"

  test("stream test") {
    DaySix.firstProblem()
  }
}
