package entity

class Grid(val height: Int,
           val width: Int,
           val chunks: List[Chunk] = List.empty) {

  lazy val cellsNum: Int = height * width

  lazy val placedCells: List[Cell] = chunks.flatMap(_.cells)

  lazy val freeCells: IndexedSeq[Cell] =
    for (x <- 0 until width;
         y <- 0 until height;
         cell = Cell(y, x)
         if !placedCells.contains(cell))
      yield cell

  lazy val isFull: Boolean = freeCells.isEmpty

  def setChunk(chunk: Chunk, startPos: Cell, optimize: Boolean = false): Option[Grid] = {
    if (optimize || canPlaceChunk(chunk, startPos)) {
      Some(new Grid(height, width, chunk.setToPos(startPos) :: chunks))
    } else None
  }

  def canPlaceChunk(chunk: Chunk, startPos: Cell): Boolean = {
    val mappedCells = chunk.setToPos(startPos).cells
    (!mappedCells.exists(invalidCell)) && placedCells.intersect(mappedCells).isEmpty
  }

  def getRotatedChunks(chunk: Chunk, startPos: Cell): List[Chunk] = {
    chunk.rotated.filter(canPlaceChunk(_, startPos))
  }

  def freeCellsNum: Int = cellsNum - placedCells.length

  def placeChunks(chunks: List[Chunk]): Option[Grid]= chunks match {
    case chunk :: Nil =>
      freeCells
        .flatMap(cell => getRotatedChunks(chunk, cell).map((cell, _)))
        .map(tuple => setChunk(tuple._2, tuple._1, optimize = true))
        .find(_.isDefined).flatten
    case chunk :: tail =>
      freeCells
        .flatMap(cell => getRotatedChunks(chunk, cell).map((cell, _)))
        .map(tuple => setChunk(tuple._2, tuple._1, optimize = true))
        .filter(_.nonEmpty).map(_.get)
        .map(grid => grid.placeChunks(tail))
        .find(_.isDefined).flatten
  }

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

  def canEqual(other: Any): Boolean = other.isInstanceOf[Grid]

  override def hashCode(): Int = {
    val state = Seq(cellsNum, placedCells, freeCells, isFull, height, width, chunks)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString: String = {
    val values = Array.ofDim[Int](height, width)
    chunks.zipWithIndex.foreach {
      case (chunk, idx) => chunk.cells.foreach(cell => values(cell.y)(cell.x) = idx + 1)
    }
    values.map(row => row.mkString("[", " ", "]")).mkString("\n", "\n", "\n")
  }

  private def invalidCell(cell: Cell): Boolean =
    (cell.y < 0 || cell.x < 0) || (cell.y >= height || cell.x >= width)
}

object Grid {
  val NON_EMPTY_CELL = 1
  val EMPTY_CELL = 0
}
