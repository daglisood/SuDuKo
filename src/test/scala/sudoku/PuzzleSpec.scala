package sudoku

import Puzzle._
import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper

import org.scalatest.wordspec.AnyWordSpec

class PuzzleSpec extends AnyWordSpec {

  var puzzle : Puzzle = new Puzzle
  puzzle = (
    (1, 2, 3, 4, 5, 6, 7, 8, 9),
    (2, 3, 4, 5, 6, 7, 8, 9, 1),
    (3, 4, 5, 6, 7, 8, 9, 1, 2),
    (4, 5, 6, 7, 8, 9, 1, 2, 3),
    (5, 6, 7, 8, 9, 1, 2, 3, 4),
    (6, 7, 8, 9, 1, 2, 3, 4, 5),
    (7, 8, 9, 1, 2, 3, 4, 5, 6),
    (8, 9, 1, 2, 3, 4, 5, 6, 7),
    (9, 1, 2, 3, 4, 5, 6, 7 ,8)
  )

  "A puzzle" should {

    "be properly initialized on creation" in {
      val newPuzzle = new Puzzle
      for (rcnt <- 0 until Puzzle.NumberRows) {
        for (ccnt <- 0 until Puzzle.RowLength) {
          newPuzzle.cell(rcnt)(ccnt) mustBe 0
        }
      }
    }

    "propoerly return mini-grids" in {
        val mg = puzzle.miniGrid(3)

        mg(0) mustBe 4
        mg(1) mustBe 5
        mg(2) mustBe 6
        mg(3) mustBe 5
        mg(4) mustBe 6
        mg(5) mustBe 7
        mg(6) mustBe 6
        mg(7) mustBe 7
        mg(8) mustBe 8
    }


    "return correct cell values" in {
      for (rcnt <- 0 until 9) {
        for (ccnt <- 0 until 9) {
          puzzle.cell(rcnt)(ccnt) mustBe (rcnt + ccnt) % 9 + 1
        }
      }
    }

    "properly set rows from a list of values" in {
        puzzle.setRow(2)(9, 8, 7, 6, 5, 4, 3, 2, 1)
        puzzle.cell(2)(0) mustBe 9
        puzzle.cell(2)(1) mustBe 8
        puzzle.cell(2)(2) mustBe 7
        puzzle.cell(2)(3) mustBe 6
        puzzle.cell(2)(4) mustBe 5
        puzzle.cell(2)(5) mustBe 4
        puzzle.cell(2)(6) mustBe 3
        puzzle.cell(2)(7) mustBe 2
        puzzle.cell(2)(8) mustBe 1
    }

    "throw exceptions for bad indices when accessing mini-grids" in {
        intercept[IllegalArgumentException] { puzzle.miniGrid(-1) }
        intercept[IllegalArgumentException] { puzzle.miniGrid(9) }
    }

    "throw exceptions for bad cell indices" in {
      intercept[IllegalArgumentException] {puzzle.cell(-1)(2)}
        intercept[IllegalArgumentException] {puzzle.cell(2)(-2) }
          intercept[IllegalArgumentException] {puzzle.cell(11)(4) }
            intercept[IllegalArgumentException] {puzzle.cell(4)(11) }
    }



    "properly set rows from an array" in {
      val row = Array[Int](9, 8, 7, 6, 5, 4, 3, 2, 1)
      puzzle.setRowFromArray(2)(row)
      puzzle.cell(2)(0) mustBe 9
      puzzle.cell(2)(1) mustBe 8
      puzzle.cell(2)(2) mustBe 7
      puzzle.cell(2)(3) mustBe 6
      puzzle.cell(2)(4) mustBe 5
      puzzle.cell(2)(5) mustBe 4
      puzzle.cell(2)(6) mustBe 3
      puzzle.cell(2)(7) mustBe 2
      puzzle.cell(2)(8) mustBe 1
    }

    "throw exceptions for bad indices when setting row values" in {
      intercept[IllegalArgumentException] {puzzle.setRow(-1)(1, 2, 3, 4, 5, 6, 7, 8, 9) }
      intercept[IllegalArgumentException] {puzzle.setRow(10)(1, 2, 3, 4, 5, 6, 7, 8, 9) }
    }
  }
}