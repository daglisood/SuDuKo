package sudoku


class Puzzle {

  val puzzle : Array[Array[Int]] = Array.ofDim(9, 9)


  def cell(rcnt : Int)(ccnt : Int) : Int = {
    require(rcnt >= 0 && rcnt < Puzzle.NumberRows, "row index not in range 0-8")
    require(ccnt >= 0 && ccnt < Puzzle.RowLength,  "column index not in range 0-8")

    puzzle(rcnt)(ccnt)
  }

  def setCell(rcnt : Int)(ccnt : Int)(value : Int) = {
    require(rcnt >= 0 && rcnt < Puzzle.NumberRows, "row index not in range 0-8")
    require(ccnt >= 0 && ccnt < Puzzle.RowLength,  "column index not in range 0-8")

    puzzle(rcnt)(ccnt) = value
  }

  def setRow(rcnt : Int)(values : (Int, Int, Int, Int, Int, Int, Int, Int, Int)) = {
    require(rcnt >= 0 && rcnt < Puzzle.NumberRows, "row index not in range 0-8")

    setCell(rcnt)(0)(values._1)
    setCell(rcnt)(1)(values._2)
    setCell(rcnt)(2)(values._3)
    setCell(rcnt)(3)(values._4)
    setCell(rcnt)(4)(values._5)
    setCell(rcnt)(5)(values._6)
    setCell(rcnt)(6)(values._7)
    setCell(rcnt)(7)(values._8)
    setCell(rcnt)(8)(values._9)
  }



  def setRowFromArray (rcnt : Int)(arr : Array[Int]) = {
    require(rcnt >= 0 && rcnt < Puzzle.NumberRows, "row index not in range 0-8")
    require (arr.length == Puzzle.RowLength, "array must be " + Puzzle.NumberRows + " items.")

    for (ccnt <- 0 until Puzzle.RowLength) {
      setCell(rcnt)(ccnt)(arr(ccnt))
    }
  }



  def row(rcnt : Int) : Array[Int] = {
    require(rcnt >= 0 && rcnt < Puzzle.NumberRows, "row index not in range 0-8")
    puzzle(rcnt)
  }


  def column(ccnt : Int) : Array[Int] = {
    require(ccnt >= 0 && ccnt < Puzzle.RowLength, "column index not in range 0-8")
    val col : Array[Int] = Array.ofDim(Puzzle.RowLength)
    for (cnt <- 0 until Puzzle.RowLength) {
      col(cnt) = puzzle(cnt)(ccnt)
    }
    col
  }


  def miniGrid(mgcnt : Int) : Array[Int] = {
    require(mgcnt >= 0 && mgcnt < Puzzle.RowLength, "mini-grid index not in range 0-8")

    var rcnt : Int = 0
    var ccnt : Int = 0

    rcnt = mgcnt match {
      case 0 => 0
      case 1 => 0
      case 2 => 0
      case 3 => 3
      case 4 => 3
      case 5 => 3
      case 6 => 6
      case 7 => 6
      case 8 => 6
    }
    ccnt = (mgcnt % 3) * 3

    val res : Array[Int] = Array.ofDim(Puzzle.RowLength)
    res(0) = puzzle(rcnt)(ccnt)
    res(1) = puzzle(rcnt)(ccnt + 1)
    res(2) = puzzle(rcnt)(ccnt + 2)
    res(3) = puzzle(rcnt + 1)(ccnt)
    res(4) = puzzle(rcnt + 1)(ccnt + 1)
    res(5) = puzzle(rcnt + 1)(ccnt + 2)
    res(6) = puzzle(rcnt + 2)(ccnt)
    res(7) = puzzle(rcnt + 2)(ccnt + 1)
    res(8) = puzzle(rcnt + 2)(ccnt + 2)

    res
  }




  override def toString () : String = {
    var res : String = ""
    puzzle.foreach { row =>
      res += row.mkString(", ") + "\n"
    }
    res
  }


  override def equals (other : Any) = {
    var areEqual = other.isInstanceOf[Puzzle]
    for (rcnt <- 0 until 9; if areEqual) {
      areEqual = areEqual && puzzle(rcnt) == other.asInstanceOf[Puzzle].row(rcnt)
    }
    areEqual
  }
}


object Puzzle {

  val RowLength   = 9  // Number of items in a row.
  val NumberRows  = 9  // Number of rows in a puzzle.
  val EmptyValue  = 0  // A value that represents an empty puzzle location.
  val ValidValues = List(1, 2, 3, 4, 5, 6, 7, 8, 9)


  def emptyVal(aval : Int) : Boolean = {
    aval < 1 || aval > 9
  }


  implicit def tupleToPuzzle(vals : ((Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int),
    (Int, Int, Int, Int, Int, Int, Int, Int, Int)
    ))
  : Puzzle = {

    val puzzle = new Puzzle
    puzzle.setRow(0)(vals._1)
    puzzle.setRow(1)(vals._2)
    puzzle.setRow(2)(vals._3)
    puzzle.setRow(3)(vals._4)
    puzzle.setRow(4)(vals._5)
    puzzle.setRow(5)(vals._6)
    puzzle.setRow(6)(vals._7)
    puzzle.setRow(7)(vals._8)
    puzzle.setRow(8)(vals._9)

    puzzle

  }


  implicit def stringToPuzzle (vals : String) : Puzzle = {
    var currentNumberString = ""
    val newPuzzle = new Puzzle

    var (rcnt, ccnt) = (0, 0)
    vals split Array(',', '\n') foreach { token =>
      var value : Int = try {
        token.trim.toInt
      } catch  {
        case _ : java.lang.NumberFormatException => EmptyValue
      }

      if (rcnt <= 8) { // don't exceed number of rows if data have more values.
        newPuzzle.setCell(rcnt)(ccnt)(value)
        ccnt += 1
        if (ccnt > 8) {
          ccnt = 0
          rcnt += 1
        }
      }
    }

    newPuzzle
  }
}


