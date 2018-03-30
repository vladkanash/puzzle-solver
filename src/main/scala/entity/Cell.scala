package entity

case class Cell(pos: (Int, Int)) {

  val y: Int = pos._1
  val x: Int = pos._2

  def +(other: Cell): Cell =
    Cell((other.pos._1 + this.pos._1, other.pos._2 + this.pos._2))

  def rotate90: Cell =
    Cell(this.x, -this.y)
}

object Cell {
  implicit def tupleToCell(tuple: (Int, Int)): Cell = new Cell(tuple)
}
