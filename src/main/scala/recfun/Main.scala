package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def buildRows(rows: List[List[Int]]): List[List[Int]] = {

      def atTargetCell(lastRowBuilt: List[Int]) = {
        def lengthOfLastRowBuilt = lastRowBuilt.length

        rows.length - 1 == r && lengthOfLastRowBuilt - 1 == c
      }

      // build row
      def buildRow(prevRow: List[Int], curRow: List[Int], curCol: Int): List[Int] = {

        def buildNextCell(prevSection: List[Int]): Int = {
          prevSection match {
            case x :: Nil => 1
            case x :: xs => x + xs.head
          }
        }

        // its rows.length because rows does not include the row being built
        if ((rows.length == r && curRow.length - 1 == c) || (curRow.length == rows.head.length + 1)) curRow
        else
        // there are two cases here
          prevRow match {
            case x :: Nil =>
              buildRow(prevRow, buildNextCell(prevRow) :: curRow, curCol + 1)
            case x :: xs =>
              if (curRow.length < 1)
                buildRow(prevRow, buildNextCell(List(prevRow.head)) :: curRow, curCol + 1)
              else
                buildRow(prevRow.tail, buildNextCell(List(prevRow.head, prevRow.tail.head)) :: curRow, curCol + 1)
          }
      }

      if (rows.length - 1 == r && rows.head.length - 1 == c) rows
      else buildRows(buildRow(rows.head, List(), 0) :: rows)
    }

    def triangleUntilCellDesired = buildRows(List(List(1)))

    triangleUntilCellDesired.head.head
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def checkParens(charsLeft: List[Char], stack: List[Char]): Boolean = {

      charsLeft match {
        case Nil => true
        case '(' :: Nil => false
        case ')' :: _ => if (stack.isEmpty) false else checkParens(charsLeft.tail, stack.tail)
        case '(' :: xs => checkParens(charsLeft.tail, '(' :: stack)
        case x :: xs => checkParens(xs, stack)
      }
    }
    checkParens(chars, List())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
// TODO sort the coins first
    // TODO: We do not need the ways argument anymore
    def helper(sum: Int, ways: Int, denoms: List[Int]):Int = {

      if (sum == 0) ways else if (denoms.isEmpty) ways
      else {
        if (denoms.head == sum)
          ways + 1
        else if (denoms.head > sum)
          ways
        else {
          helper(sum - denoms.head, 0, denoms) + helper(sum, 0, denoms.tail)
        }
      }
    }
      helper(money, 0, coins.sorted)
  }
}
