package entity

import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import scala.util.Random

class GridTest extends PropSpec with PropertyChecks with Matchers {

  private val CHUNK_MAX_SIZE = 6
  private val GRID_MAX_SIZE = 10
  private val random = new Random

  private def createRandomChunk(size: Int): Chunk = {
    val directionMap = Map(
      0 -> (0, -1),
      1 -> (1, 0),
      2 -> (0, 1),
      3 -> (-1, 0))

    def appendCells(cells: List[(Int, Int)], cellsLeft: Int): List[(Int, Int)] = {
      val cell = cells(random.nextInt(cells.length))
      val direction = directionMap(random.nextInt(4))
      val newCell = (cell._1 + direction._1, cell._2 + direction._2)
      val newList = (newCell :: cells).distinct
      if (cellsLeft <= 0) newList else appendCells(newList, cellsLeft - 1)
    }

    new Chunk(appendCells(List((0, 0)), size))
  }

  private val posGenerator =
    for (x <- Gen.choose(0, GRID_MAX_SIZE); y <- Gen.choose(0, GRID_MAX_SIZE))
      yield (x, y)

  private val chunksGenerator =
    for (chunkSize <- Gen.choose(0, CHUNK_MAX_SIZE))
      yield createRandomChunk(chunkSize)

  private val gridGenerator =
    for (width <- Gen.choose(1, GRID_MAX_SIZE); height <- Gen.choose(1, GRID_MAX_SIZE))
      yield new Grid(height, width)

  property("You should be able to place chunks on a grid") {
    forAll(chunksGenerator, gridGenerator, posGenerator) {
      (chunk: Chunk, grid: Grid, startPos: (Int, Int)) => {
        val newGrid = grid.setChunk(chunk, startPos)
        if (newGrid != grid)
          grid.freeCellsNum - newGrid.freeCellsNum shouldEqual chunk.cells.length
        else succeed
      }
    }
  }

  property("Free cells field should contain all free cells on the grid") {
    forAll(chunksGenerator, gridGenerator, posGenerator) {
      (chunk: Chunk, grid: Grid, startPos: (Int, Int)) => {
        val newGrid = grid.setChunk(chunk, startPos)
        if (newGrid != grid)
          newGrid.freeCells intersect newGrid.placedCells shouldEqual List.empty
        else succeed
      }
    }
  }

  property("canPlaceChunk() should check id the chunk fits to the current grid") {
    forAll(chunksGenerator, gridGenerator, posGenerator) {
      (chunk: Chunk, grid: Grid, startPos: (Int, Int)) => {
        val newGrid = grid.setChunk(chunk, startPos)
        (newGrid != grid) shouldEqual grid.canPlaceChunk(chunk, startPos)
      }
    }
  }

}
