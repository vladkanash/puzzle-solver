package entity

class Chunk(val cells: List[(Int, Int)]) {

  def setToPos(startPos: (Int, Int)): Chunk =
    new Chunk(cells.map(pos => (startPos._1 + pos._1, startPos._2 + pos._2)))

  override def toString: String = {
    val minY = cells.minBy(cell => cell._1)._1
    val maxY = cells.maxBy(cell => cell._1)._1

    val minX = cells.minBy(cell => cell._2)._2
    val maxX = cells.maxBy(cell => cell._2)._2

    val height = maxY - minY + 1
    val width = maxX - minX + 1

    val grid = new Grid(height, width)
    val startPos = (height - maxY - 1, width - maxX - 1)

    grid.setChunk(this, startPos).toString
  }

}
