package entity

import util.GridTestSuite

class GridTest extends GridTestSuite {

  gridTest("You should be able to place chunks on a grid") {
    (chunks, grid, startPos) => {
      val chunk = chunks.head
      val newGrid = grid.placeChunk(chunk, startPos)
      if (newGrid.isDefined)
        grid.freeCellsNum - newGrid.get.freeCellsNum shouldEqual chunk.cells.length
      else succeed
    }
  }

  gridTest("Free cells field should contain all free cells on the grid") {
    (chunks, grid, startPos) => {
      val chunk = chunks.head
      val newGrid = grid.placeChunk(chunk, startPos)
      if (newGrid.isDefined)
        newGrid.get.freeCells intersect newGrid.get.placedCells shouldEqual List.empty
      else succeed
    }
  }

  gridTest("canPlaceChunk() should check id the chunk fits to the current grid") {
    (chunks, grid, startPos) => {
      val chunk = chunks.head
      val newGrid = grid.placeChunk(chunk, startPos)
      newGrid.isDefined shouldEqual grid.canPlaceChunk(chunk, startPos)
    }
  }

  gridTest("placeChunks() should try to fill the grid with chunks") {
    (chunks, grid, _) => {
      grid.placeChunks(chunks).collect({
        case newGrid => newGrid.freeCellsNum shouldBe < (grid.freeCellsNum)
      }).getOrElse(succeed)
    }
  }

  gridTest("If placeChunks() succeeds, the number of placed cells should be the same as the number of cells in chunk list") {
    (chunks, grid, _) => {
      grid.placeChunks(chunks).collect({
        case newGrid => newGrid.chunks.map(_.cells.length).sum shouldEqual chunks.map(_.cells.length).sum
      }).getOrElse(succeed)
    }
  }

  gridTest("freeChunks should return the list of free chunks on the grid") {
    (chunks, grid, _) => {
      grid.placeChunks(chunks).collect({
        case newGrid => newGrid.freeChunks.flatMap(_.cells).sorted shouldEqual newGrid.freeCells.sorted
      }).getOrElse(succeed)
    }
  }

  chunkTest("360 degrees rotation should return the same chunk") {
    (chunks) => {
      val chunk = chunks.head
      chunk.rotated.last.rotated.head shouldEqual chunk
    }
  }
}
