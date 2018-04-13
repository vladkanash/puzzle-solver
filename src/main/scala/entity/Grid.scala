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

  lazy val freeChunks: List[Chunk] = {

    def buildChunk(old: List[Cell]): List[Cell] = {
      def getAllNeighboursForCell(cell: Cell): List[Cell] = {
        val x = cell.x
        val y = cell.y
        List(Cell(y - 1, x), Cell(y, x - 1), Cell(y, x), Cell(y, x + 1), Cell(y + 1, x))
      }

      def getValidNeighboursForCell(cell: Cell): List[Cell] =
        getAllNeighboursForCell(cell).filterNot(invalidCell).filter(freeCells.contains)

      def appendNeighbours(cells: List[Cell]): List[Cell] =
        cells.flatMap(getValidNeighboursForCell).distinct

      val updated = appendNeighbours(old)
      if (updated.lengthCompare(old.length) == 0)
        old
      else
        buildChunk(updated)
    }

    def createChunk(cell: Cell): Chunk =
      Chunk(buildChunk(List(cell)), startPos = cell)

    def buildChunks(freeCells: List[Cell], chunks: List[Chunk]): List[Chunk] = freeCells match {
      case Nil => chunks
      case head :: tail =>
        val newChunk = createChunk(head)
        val cellsLeft = tail diff newChunk.cells
        buildChunks(cellsLeft, newChunk :: chunks)
    }

    buildChunks(freeCells.toList, List.empty)
  }

  def placeChunk(chunk: Chunk, startPos: Cell, skipCheck: Boolean = false): Option[Grid] = {
    if (skipCheck || canPlaceChunk(chunk, startPos)) {
      Some(new Grid(height, width, chunk.setToPos(startPos) :: chunks))
    } else None
  }

  def placeChunks(chunks: List[Chunk]): Option[Grid] = chunks match {
    case Nil => Some(this)
    case chunk :: tail =>
      freeCells
        .flatMap(cell => getRotatedChunks(chunk, cell).map((cell, _)))
        .map(tuple => new Grid(height, width, tuple._2.setToPos(tuple._1) :: this.chunks))
        .collectFirst({
          case grid if grid.placeChunks(tail).isDefined => grid.placeChunks(tail).get
        })
  }

  def canPlaceChunk(chunk: Chunk, startPos: Cell): Boolean = {
    val mappedCells = chunk.setToPos(startPos).cells
    (!mappedCells.exists(invalidCell)) && placedCells.intersect(mappedCells).isEmpty
  }

  override def toString: String = {
    val values = Array.ofDim[Int](height, width)
    chunks.zipWithIndex.foreach {
      case (chunk, idx) => chunk.cells.foreach(cell => values(cell.y)(cell.x) = idx + 1)
    }
    values.map(row => row
      .map(cell => if (cell != 0) (cell + 64).toChar else '-')
      .mkString("[", " ", "]"))
      .mkString("\n", "\n", "\n")
  }

  private def invalidCell(cell: Cell): Boolean =
    (cell.y < 0 || cell.x < 0) || (cell.y >= height || cell.x >= width)

  private def getRotatedChunks(chunk: Chunk, startPos: Cell): List[Chunk] = {
    chunk.rotated.filter(canPlaceChunk(_, startPos))
  }
}
