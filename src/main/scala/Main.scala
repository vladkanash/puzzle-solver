import entity.{Chunk, Grid}

object Main extends App {
  val grid = new Grid(5,6)

  val chunks = List(
    Chunk(List((0,0), (0,1), (1,0))),
    Chunk(List((0,0), (0,1), (1,0))),
    Chunk(List((0,0), (0,1), (1,0))),
    Chunk(List((0,0), (0,1), (1,0))),
    Chunk(List((0,0), (0,1), (1,0), (1,1))),
    Chunk(List((0,0), (1,0), (2,0), (3,0))),
    Chunk(List((0,0), (1,0), (2,0), (3,0), (4,0))),
    Chunk(List((0,0), (1,0), (2,0))),
    Chunk(List((0,0), (1,0)))
  )

  println(grid.placeChunks(chunks))
}
