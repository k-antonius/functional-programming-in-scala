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

      def buildRows(rows : List[List[Int]]): List[List[Int]] = {

        def atTargetCell(lastRowBuilt: List[Int])= {
          def lengthOfLastRowBuilt = lastRowBuilt.length
          rows.length - 1 == r && lengthOfLastRowBuilt - 1 == c
        }

        // build row
        def buildRow(prevRow: List[Int], curRow: List[Int], curCol: Int): List[Int] = {

          def buildNextCell(prevSection : List[Int] ): Int = {
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
    def balance(chars: List[Char]): Boolean = ???
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }
