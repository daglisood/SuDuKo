package sudoku


object ValidateSuDoKu {


  def apply (puzzle : Puzzle) : Boolean = {
    val rowsResponse = checkRows(puzzle)
    val columnsResponse = checkColumns(puzzle)
    val miniGridsResponse = checkMiniGrids(puzzle)

    rowsResponse && columnsResponse && miniGridsResponse
  }


  private def checkRows (puzzle : Puzzle) : Boolean = {
    var valid = true
    for (cnt <- 0 until 9; if (valid)) {
      valid = valid && checkUnique (puzzle.row(cnt))
    }
    valid
  }


  private def checkColumns (puzzle : Puzzle) : Boolean = {
    var valid = true
    for (cnt <- 0 until 9; if(valid)) {
      valid = valid && checkUnique (puzzle.column(cnt))
    }
    valid
  }


  private def checkMiniGrids (puzzle : Puzzle) : Boolean = {
    var valid = true
    for (cnt <- 0 until 9; if(valid)) {
      valid = valid && checkUnique (puzzle.miniGrid(cnt))
    }
    valid
  }


  def checkUnique(arr : Array[Int]) : Boolean = {
    arr.groupBy(identity).collect{ case (x,Array(_,_,_*)) => x}.isEmpty
  }
}


