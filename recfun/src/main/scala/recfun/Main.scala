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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r < 2) {
      1
    } else ( pascal(c-1, r-1) + pascal(c, r-1))
  }

  /**
   * Exercise 2
   *
   * True for empty chars as there's no problem with unbalanced parenthesis then.
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) {
      true
    } else if (closerStarts(chars) || openerCloses(chars)) {
      false
    } else balanceValidListOf(chars, 0)
  }

  def closerStarts(chSeq : List[Char]) : Boolean = firstParenthesis(chSeq)         == ')'
  def openerCloses(chSeq : List[Char]) : Boolean = firstParenthesis(chSeq.reverse) == '('

  def firstParenthesis(chSeq : List[Char]) : Char = chSeq.head match {
    case '(' => '('
    case ')' => ')'
    case _ => firstParenthesis(chSeq.tail)
  }

  
  def balanceValidListOf(chars: List[Char], stackCount : Int) : Boolean = {
    if (stackCount < 0 || (chars.isEmpty && stackCount > 0)) {
      false
    } else if (chars.isEmpty && stackCount == 0) { 
      true
    } else chars.head match {
        case '(' => balanceValidListOf(chars.tail, stackCount + 1) 
        case ')' => balanceValidListOf(chars.tail, stackCount - 1) 
        case _   => balanceValidListOf(chars.tail, stackCount) 
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    countWays(money, coins.sorted)
  }
  
  def countWays(money:Int, coins: List[Int]) : Int = {
    if (money < 0 || coins.isEmpty) {
        0
    } else if (money == 0) {
      1
    } else {
      countWays(money, coins) + countWays(money - coins.head, coins.tail)
    }
  }

}
