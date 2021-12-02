package Utils

object ReadInputFile {
  val root = "src/main/scala/"
  val source = scala.io.Source.fromFile(root)

  def toList(filename: String): List[Integer] = {
    val source = scala.io.Source.fromFile(filename)
    try source.getLines().toList.map(_.toInt) finally source.close()
  }

  def toPair(filename: String): List[(String, Integer)] = {
    val source = scala.io.Source.fromFile(filename)
    try source.getLines().toList.map{
      line =>
        val splitLine: Array[String] = line.split(" ")
        (splitLine(0), splitLine(1).toInt)
    } finally source.close()
  }
}
