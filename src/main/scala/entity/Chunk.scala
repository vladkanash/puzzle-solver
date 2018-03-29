package entity

class Chunk(val cells: List[Cell]) {

  def setToPos(startPos: Cell): Chunk =
    new Chunk(cells.map(_ + startPos))

  override def toString: String = {
    val minY = cells.minBy(_.y).y
    val maxY = cells.maxBy(_.y).y

    val minX = cells.minBy(_.x).x
    val maxX = cells.maxBy(_.x).x

    val height = maxY - minY + 1
    val width = maxX - minX + 1

    val grid = new Grid(height, width)
    val startPos = (height - maxY - 1, width - maxX - 1)

    grid.setChunk(this, startPos).toString
  }

}
