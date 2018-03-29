package entity

case class Cell(pos: (Int, Int)) {

  def + (other: Cell): Cell =
    new Cell((other.pos._1 + this.pos._1, other.pos._2 + this.pos._2))

  val y: Int = pos._1
  val x: Int = pos._2

}

object Cell {
  implicit def tupleToCell(tuple: (Int, Int)): Cell = new Cell(tuple)
}
