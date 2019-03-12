
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

    loop(n = m, acc = 1)
  }

}

print(session.factorial(4))

object session2 {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc // Identity sum
      else loop(a + 1, acc + f(a))
    }

    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else product(f)(a + 1, b) * f(a)
  }

  def factorial(n: Int) = product(x => x)(0, n)

  def general(g: (Int, Int) => Int, id: Int)(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) id
    else g(product(f)(a + 1, b), f(a))
  }

  def product2(f: Int => Int)(a: Int, b: Int): Int = general((x: Int, y: Int) => x * y, 1)(f)(a, b)

  def factorial2(n: Int) = product2(x => x)(1, n)

}

val x = session2.sum(x => x * x)(0, 4)
val y = session2.product(x => x * x)(1, 4)
val y2 = session2.product2(x => x * x)(1, 4)
val factorial = session2.factorial2(5)

