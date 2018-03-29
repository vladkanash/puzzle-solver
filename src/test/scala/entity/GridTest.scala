package entity

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}

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

    def appendCells(cells: List[Cell], cellsLeft: Int): List[Cell] = {
      val cell = cells(random.nextInt(cells.length))
      val direction = directionMap(random.nextInt(4))
      val newCell = cell + direction
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

  def gridTest(testName: String)(test: (Chunk, Grid, (Int, Int)) => Assertion): Unit = {
    property(testName) {
      forAll(chunksGenerator, gridGenerator, posGenerator) {
        (chunk: Chunk, grid: Grid, startPos: (Int, Int)) => {
          test(chunk, grid, startPos)
        }
      }
    }
  }

  gridTest("You should be able to place chunks on a grid") {
    (chunk, grid, startPos) => {
      val newGrid = grid.setChunk(chunk, startPos)
      if (newGrid != grid)
        grid.freeCellsNum - newGrid.freeCellsNum shouldEqual chunk.cells.length
      else succeed
    }
  }

  gridTest("Free cells field should contain all free cells on the grid") {
    (chunk, grid, startPos) => {
      val newGrid = grid.setChunk(chunk, startPos)
      if (newGrid != grid)
        newGrid.freeCells intersect newGrid.placedCells shouldEqual List.empty
      else succeed
    }
  }

  gridTest("canPlaceChunk() should check id the chunk fits to the current grid") {
    (chunk, grid, startPos) => {
      val newGrid = grid.setChunk(chunk, startPos)
      (newGrid != grid) shouldEqual grid.canPlaceChunk(chunk, startPos)
    }
  }

  gridTest("placeChunks() should try to fill the grid with chunks") {
    (chunk, grid, _) => {
      val chunks = List(chunk, chunk, chunk, chunk)
      val newGrid = grid.placeChunks(chunks)
      if (newGrid != grid)
        newGrid.freeCellsNum shouldBe < (grid.freeCellsNum)
      else
        newGrid.placedCells.length shouldBe < (chunks.flatMap(_.cells).length)
    }
  }

}
