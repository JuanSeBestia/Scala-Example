
object session {
  def date = java.time.LocalDateTime.now + ""

  def printDate2Value(x: String) {
    print(x)
    Thread.sleep(1000) // wait for 1000 millisecond
    print(x)
  }

  def printDate2Name(x: => String) {
    print(x)
    Thread.sleep(1000) // wait for 1000 millisecond
    print(x)
  }



  def factorial(m: Int) = {
    def loop(acc: Int, n: Int): Int = {
      if (n == 0) acc
      else loop(acc * n, n - 1)
    }

    loop(n=m,acc=1)
  }

}

print(session.factorial(4))