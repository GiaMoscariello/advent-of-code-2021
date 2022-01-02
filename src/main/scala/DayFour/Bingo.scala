package DayFour

case class Bingo(drawNumbers: List[String], tables: List[Table]) {
  def hasOneTable() = tables.length == 1

  def numberExtraction(number: Int): Bingo = {
    val updatedTables = tables.map {
      table => Table.crossTable(number, table)
    }
    Bingo(drawNumbers, updatedTables)
  }

  def winningTable(table: Table): Boolean = {
    table.values.exists(line => line.forall(x => x == Int.MinValue)) ||
      table.values.transpose.exists(line => line.forall(x => x == Int.MinValue))
  }

  def BINGO(): Option[Table] = {
    tables.find{table => winningTable(table)}
  }

  def extractNumberAndDeleteWinningTable(number: Int): Bingo = {
    val updatedTables = tables.map(table => Table.crossTable(number, table))
    val noWinningTable = updatedTables.filter(table => !winningTable(table))
    Bingo(drawNumbers, noWinningTable)
  }
}

case class Table(values: Seq[List[Int]], id: Int, crossed: List[Int] = List()) {
  def numberIsPresent(num: Int): Boolean = {
    values.exists(line => line.contains(num))
  }
}

object Table {
  def crossTable(number: Int, table: Table): Table = {
    Table(table.values
      .map(line => line.map( x => if (x == number) Int.MinValue else x)),
      table.id, number :: table.crossed)
  }
}