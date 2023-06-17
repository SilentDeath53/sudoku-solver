object SudokuSolver extends App {

  val emptyCell: Int = 0

  def printBoard(board: Array[Array[Int]]): Unit = {
    for (i <- 0 until 9) {
      for (j <- 0 until 9) {
        print(board(i)(j) + " ")
      }
      println()
    }
    println()
  }

  def isValid(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
    for (i <- 0 until 9) {
      if (board(row)(i) == num || board(i)(col) == num)
        return false
    }

    val startRow = (row / 3) * 3
    val startCol = (col / 3) * 3
    for (i <- startRow until startRow + 3) {
      for (j <- startCol until startCol + 3) {
        if (board(i)(j) == num)
          return false
      }
    }

    true
  }

  def solveSudoku(board: Array[Array[Int]]): Boolean = {
    for (row <- 0 until 9) {
      for (col <- 0 until 9) {
        if (board(row)(col) == emptyCell) {
          for (num <- 1 to 9) {
            if (isValid(board, row, col, num)) {
              board(row)(col) = num
              if (solveSudoku(board))
                return true
              else
                board(row)(col) = emptyCell
            }
          }
          return false
        }
      }
    }
    true
  }

  val board: Array[Array[Int]] = Array(
    Array(5, 3, 0, 0, 7, 0, 0, 0, 0),
    Array(6, 0, 0, 1, 9, 5, 0, 0, 0),
    Array(0, 9, 8, 0, 0, 0, 0, 6, 0),
    Array(8, 0, 0, 0, 6, 0, 0, 0, 3),
    Array(4, 0, 0, 8, 0, 3, 0, 0, 1),
    Array(7, 0, 0, 0, 2, 0, 0, 0, 6),
    Array(0, 6, 0, 0, 0, 0, 2, 8, 0),
    Array(0, 0, 0, 4, 1, 9, 0, 0, 5),
    Array(0, 0, 0, 0, 8, 0, 0, 7, 9)
  )

  println("Sudoku board:")
  printBoard(board)

  if (solveSudoku(board)) {
    println("Solution:")
    printBoard(board)
  } else {
    println("No solution exists.")
  }
}
