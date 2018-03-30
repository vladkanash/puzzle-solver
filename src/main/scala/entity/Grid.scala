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

  lazy val freeCellsNum: Int = cellsNum - placedCells.length

  def setChunk(chunk: Chunk, startPos: Cell, skipCheck: Boolean = false): Option[Grid] = {
    if (skipCheck || canPlaceChunk(chunk, startPos)) {
      Some(new Grid(height, width, chunk.setToPos(startPos) :: chunks))
    } else None
  }

  def canPlaceChunk(chunk: Chunk, startPos: Cell): Boolean = {
    val mappedCells = chunk.setToPos(startPos).cells
    (!mappedCells.exists(invalidCell)) && placedCells.intersect(mappedCells).isEmpty
  }

  def placeChunks(chunks: List[Chunk]): Option[Grid] = chunks match {
    case Nil => Some(this)
    case chunk :: tail =>
      freeCells
        .flatMap(cell => getRotatedChunks(chunk, cell).map((cell, _)))
        .map(tuple => setChunk(tuple._2, tuple._1, skipCheck = true))
        .filter(_.nonEmpty).map(_.get)
        .find(grid => grid.placeChunks(tail).isDefined)
        .flatMap(grid => grid.placeChunks(tail))
  }

  override def toString: String = {
    val values = Array.ofDim[Int](height, width)
    chunks.zipWithIndex.foreach {
      case (chunk, idx) => chunk.cells.foreach(cell => values(cell.y)(cell.x) = idx + 1)
    }
    values.map(row => row.mkString("[", " ", "]")).mkString("\n", "\n", "\n")
  }

  private def getRotatedChunks(chunk: Chunk, startPos: Cell): List[Chunk] = {
    chunk.rotated.filter(canPlaceChunk(_, startPos))
  }

  private def invalidCell(cell: Cell): Boolean =
    (cell.y < 0 || cell.x < 0) || (cell.y >= height || cell.x >= width)
}
