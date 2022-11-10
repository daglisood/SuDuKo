package sudoku

import org.scalatest.matchers.must.Matchers.convertToAnyMustWrapper
import org.scalatest.wordspec.AnyWordSpec

class ValidateSuDoKuSpec extends AnyWordSpec {


  "A puzzle" should {

    "verify if the rows, columns, and mini-grids have only unique values" in {
      val puzzle  = (
        (1, 2, 3, 4, 5, 6, 7, 8, 9),
        (4, 5, 6, 7, 8, 9, 1, 2, 3),
        (7, 8, 9, 1, 2, 3, 4, 5, 6),
        (2, 3, 4, 5, 6, 7, 8, 9, 1),
        (5, 6, 7, 8, 9, 1, 2, 3, 4),
        (8, 9, 1, 2, 3, 4, 5, 6, 7),
        (3, 4, 5, 6, 7, 8, 9, 1, 2),
        (6, 7, 8, 9, 1, 2, 3, 4, 5),
        (9, 1, 2, 3, 4, 5, 6, 7 ,8)
      )
      ValidateSuDoKu(puzzle) mustBe true
    }

    "not verify if there are duplicates in a single row" in {
      val puzzle  = (
        (1, 1, 1, 4, 5, 6, 7, 8, 9),
        (4, 5, 6, 7, 8, 9, 1, 2, 3),
        (7, 8, 9, 1, 2, 3, 4, 5, 6),
        (2, 3, 4, 5, 6, 7, 8, 9, 1),
        (5, 6, 7, 8, 9, 1, 2, 3, 4),
        (8, 9, 1, 2, 3, 4, 5, 6, 7),
        (3, 4, 5, 6, 7, 8, 9, 1, 2),
        (6, 7, 8, 9, 1, 2, 3, 4, 5),
        (9, 1, 2, 3, 4, 5, 6, 7 ,8)
      )
      ValidateSuDoKu(puzzle) mustBe false
    }

    "not verify if there are duplicates in a column" in {
      val puzzle = (
        (3, 2, 1, 4, 5, 6, 7, 8, 9),
        (5, 4, 6, 7, 8, 9, 1, 2, 3),
        (7, 8, 9, 1, 2, 3, 4, 5, 6),
        (3, 2, 4, 5, 6, 7, 8, 9, 1),
        (5, 6, 7, 8, 9, 1, 2, 4, 3),
        (8, 9, 1, 2, 3, 4, 5, 7, 6),
        (3, 4, 5, 6, 7, 8, 1, 9, 2),
        (6, 7, 8, 9, 2, 1, 3, 4, 5),
        (9, 1, 2, 3, 4, 5, 6, 7, 8)
      )
      ValidateSuDoKu(puzzle) mustBe false
    }


    "not verify if there are duplicates in a mini-grid" in {
      val puzzle = (
        (4, 5, 6, 7, 8, 9, 1, 2, 3),
        (7, 8, 9, 1, 2, 3, 4, 5, 6),
        (5, 6, 7, 8, 9, 1, 2, 3, 4),
        (1, 2, 3, 4, 5, 6, 7, 8, 9),
        (2, 3, 4, 5, 6, 7, 8, 9, 1),
        (8, 9, 1, 2, 3, 4, 5, 6, 7),
        (3, 4, 5, 6, 7, 8, 9, 1, 2),
        (6, 7, 8, 9, 1, 2, 3, 4, 5),
        (9, 1, 2, 3, 4, 5, 6, 7 ,8)
      )
      ValidateSuDoKu(puzzle) mustBe false
    }

  }
}
