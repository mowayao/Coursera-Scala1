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
      if(r==0 || c==0 || r==c) 1
      else pascal(c,r-1)+pascal(c-1,r-1)
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
      def rec(cnt: Int, chars: List[Char]): Boolean={
        if(cnt < 0) false
        else if(chars.isEmpty) true
        else if(chars.head=='(') rec(cnt+1,chars.tail)
        else if(chars.head==')') rec(cnt+1,chars.tail)
        else rec(cnt,chars.tail)
      }
      rec(0,chars)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if(money==0) 1
      if(money < 0) 0
      countChange(money-coins.head,coins)+countChange(money-coins.head,coins.tail)
  }
}
