package recfun

import scala.annotation.tailrec

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
      if (c==0 || c == r) 1
      else pascal(c, r-1) + pascal(c-1 , r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      @tailrec
      def search(acc: Int, chars: List[Char]): Boolean = {
        // end of string. Return true if no pending '(' characters are left.
        if (chars.isEmpty)
          acc == 0
        // '(' character found -> increase accumulator and keep searching
        else if (chars.head == '(')
          search(acc + 1, chars.tail)
        // ')' character found
        else if (chars.head == ')')
          // ')' character found while pending '(' character -> decrease accumulator and keep searching
          if (acc > 0)
            search(acc - 1, chars.tail)
          // No pending ')' character. Return false.
          else
            false
        // Regular character -> keep searching
        else
          search(acc, chars.tail)
      }

      search(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else
        countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
