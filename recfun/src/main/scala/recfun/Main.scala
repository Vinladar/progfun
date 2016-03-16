package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c==0 ) 1
    else if(r<c) 0
    else pascal(c-1,r-1)+pascal(c,r-1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char], parentsCount:Int=0): Boolean = {
    def count (char:Char) =
      if (char=='(') 1 else if (char==')') -1 else 0
    if(chars.isEmpty)
      parentsCount==0
    else if (parentsCount< 0) false
    else balance(chars.tail, count(chars.head)+parentsCount)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty) 0
    else if(money < coins.head) countChange(money, coins.tail)
    else if(money==coins.head) 1 + countChange(money, coins.tail)
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }
}
