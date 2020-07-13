package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1 else
      if (c == r) 1 else
        pascal(c - 1, r - 1) + pascal(c, r - 1)

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance2(chars: List[Char], openParentheses: Int): Boolean = {
      if (chars.isEmpty) openParentheses == 0 else
        if (chars.head == '(') balance2(chars.tail, openParentheses + 1) else
          if (chars.head == ')' && openParentheses == 0) false else
            if (chars.head == ')')  balance2(chars.tail, openParentheses - 1) else

      balance2(chars.tail, openParentheses)
    }

    balance2(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1 else
        if (money < 0) 0 else
         if(money > 0 && coins.isEmpty) 0 else
           countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
