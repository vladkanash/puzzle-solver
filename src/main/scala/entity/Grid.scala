package entity

class Grid(val height: Int, val width: Int, val chunks: List[Chunk] = List.empty) {

  val cellsNum: Int = height * width

  def setChunk(chunk: Chunk, startPos: (Int, Int)): Grid = {
    val mappedChunk = chunk.setToPos(startPos)

    if (mappedChunk.cells.exists(invalidCell))
      this
    else if (placedCells.intersect(mappedChunk.cells).isEmpty)
      new Grid(height, width, mappedChunk :: chunks)
    else this
  }

  def freeCellsNum: Int = cellsNum - placedCells.length

  def canPlaceChunk(chunk: Chunk, startPos: (Int, Int)): Boolean =
    !chunk.setToPos(startPos).cells.exists(invalidCell)

  lazy val placedCells: List[(Int, Int)] = chunks.flatMap(chunk => chunk.cells)

  lazy val freeCells: List[(Int, Int)] =
    (for (x <- 0 until width; y <- 0 until height if !placedCells.contains((y, x))) yield (y, x)).toList

  lazy val isFull: Boolean = freeCells.isEmpty

  private def invalidCell(cell: (Int, Int)): Boolean =
    (cell._1 < 0 || cell._2 < 0) || (cell._1 >= height || cell._2 >= width)

  override def toString: String = {
    val values = Array.ofDim[Int](height, width)
    chunks.zipWithIndex.foreach {
      case (chunk, idx) => chunk.cells.foreach(cell => values(cell._1)(cell._2) = idx + 1)
    }
    values.map(row => row.mkString("[", " ", "]")).mkString("\n", "\n", "\n")
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[Grid]

  override def equals(other: Any): Boolean = other match {
    case that: Grid =>
      (that canEqual this) &&
        cellsNum == that.cellsNum &&
        placedCells == that.placedCells &&
        freeCells == that.freeCells &&
        height == that.height &&
        width == that.width &&
        chunks == that.chunks
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(cellsNum, placedCells, freeCells, isFull, height, width, chunks)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Grid {
  val NON_EMPTY_CELL = 1
  val EMPTY_CELL = 0
}
