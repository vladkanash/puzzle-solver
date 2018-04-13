package entity

case class Cell(pos: (Int, Int)) {

  val y: Int = pos._1
  val x: Int = pos._2

  def +(other: Cell): Cell =
    Cell((other.pos._1 + this.pos._1, other.pos._2 + this.pos._2))

  def -(other: Cell): Cell =
    Cell((other.pos._1 - this.pos._1, other.pos._2 - this.pos._2))

  def rotate90: Cell =
    Cell(this.x, -this.y)

  def canEqual(a: Any): Boolean = a.isInstanceOf[Cell]

  override def equals(that: Any): Boolean = that match {
    case that: Cell => that.canEqual(this) && this.pos == that.pos
    case _ => false
  }
}

object Cell {
  implicit def tupleToCell(tuple: (Int, Int)): Cell = new Cell(tuple)
  implicit def cellOrdering: Ordering[Cell] = Ordering.by(_.pos)
}
