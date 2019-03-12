package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("CountingChange")
    println(countChange(4, List(1, 2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0) 0
    else if (c > r) 0
    else if (r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(stack: Int, chars: List[Char]): Boolean = {
      if (stack < 0) false
      else if (chars.isEmpty)
        if (stack == 0) true else false
      else if (chars.head == '(') loop(stack + 1, chars.tail)
      else if (chars.head == ')') loop(stack - 1, chars.tail)
      else loop(stack, chars.tail)

    }

    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int], change: List[Int]): Int = {
      val accChange = change.sum
      if (accChange == money) 1
      else if (accChange > money) 0
      else {
        val results = coins.filter(
          x => if (change.isEmpty) true else x >= change.head
        ).map(a => loop(money, coins, a :: change))
        results.sum
      }

    }

    if (money <= 0) 0
    else if (coins.isEmpty) 0
    else loop(money, coins.sorted, Nil)
  }

}
