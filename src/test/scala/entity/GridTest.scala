package entity

import util.GridTestSuite

class GridTest extends GridTestSuite {

  gridTest("You should be able to place chunks on a grid") {
    (chunk, grid, startPos) => {
      val newGrid = grid.setChunk(chunk, startPos)
      if (newGrid.isDefined)
        grid.freeCellsNum - newGrid.get.freeCellsNum shouldEqual chunk.cells.length
      else succeed
    }
  }

  gridTest("Free cells field should contain all free cells on the grid") {
    (chunk, grid, startPos) => {
      val newGrid = grid.setChunk(chunk, startPos)
      if (newGrid.isDefined)
        newGrid.get.freeCells intersect newGrid.get.placedCells shouldEqual List.empty
      else succeed
    }
  }

  gridTest("canPlaceChunk() should check id the chunk fits to the current grid") {
    (chunk, grid, startPos) => {
      val newGrid = grid.setChunk(chunk, startPos)
      newGrid.isDefined shouldEqual grid.canPlaceChunk(chunk, startPos)
    }
  }

  gridTest("placeChunks() should try to fill the grid with chunks") {
    (chunk, grid, _) => {
      val chunks = List(chunk, chunk, chunk)
      val newGrid = grid.placeChunks(chunks)
      if (newGrid.isDefined)
        newGrid.get.freeCellsNum shouldBe < (grid.freeCellsNum)
      else succeed
    }
  }
}
