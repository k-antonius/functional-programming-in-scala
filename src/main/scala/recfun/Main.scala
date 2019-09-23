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
        // build row
        def buildRow(prevRow: List[Int], curRow: List[Int], curCol: Int): List[Int] = {

          def buildNextCell(prevSection : List[Int] ): Int = {
            prevSection match {
              case x :: Nil => 1
              case x :: xs => x + xs.head
            }
          }

            if ((rows.length == r && curCol == c) || (curRow.length == prevRow.length + 1)) curRow
            else
              buildRow(prevRow.tail, buildNextCell(List(prevRow.head, prevRow.tail.head)) :: curRow, curCol + 1)
          }

        rows match {
          case rows.length == r => rows
          case _ => buildRows(buildRow(rows.head, List(), 0) :: rows)
        }
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
