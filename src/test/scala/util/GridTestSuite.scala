package util

import entity.{Cell, Chunk, Grid}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}

import scala.util.Random

trait GridTestSuite extends PropSpec with PropertyChecks with Matchers {

  private val CHUNK_MAX_SIZE = 6
  private val GRID_MAX_SIZE = 10
  private val random = new Random

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

    Chunk(appendCells(List((0, 0)), size))
  }

}
