package entity

case class Chunk(cells: List[Cell], symmType: Symmetry.Value = Symmetry.None) {

  lazy val rotated: List[Chunk] = symmType match {
    case Symmetry.None =>
      List(
        this,
        this.rotate90,
        this.rotate90.rotate90,
        this.rotate90.rotate90.rotate90
      )
    case Symmetry.Single =>
      List(this, this.rotate90)
    case Symmetry.BiAxial =>
      List(this)
  }

  def setToPos(startPos: Cell): Chunk =
    Chunk(cells.map(_ + startPos), symmType)

  override def toString: String = {
    val minY = cells.minBy(_.y).y
    val maxY = cells.maxBy(_.y).y

    val minX = cells.minBy(_.x).x
    val maxX = cells.maxBy(_.x).x

    val height = maxY - minY + 1
    val width = maxX - minX + 1

    val grid = new Grid(height, width)
    val startPos = (height - maxY - 1, width - maxX - 1)

    grid.placeChunk(this, startPos).get.toString
  }

  private def rotate90: Chunk =
    Chunk(this.cells.map(_.rotate90), symmType)
}
