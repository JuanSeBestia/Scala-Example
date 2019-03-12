package week3

class Rational(x: Int, y: Int) {

  require(y != 0, "denominator need different of 0")

  def date = java.time.LocalDateTime.now + ""

  // println("new Rational  " + date)

  // Second constructor
  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  val number = x / g

  val denom = y / g

  def add(that: Rational) = new Rational(
    number * that.denom + that.number * denom
    , (denom * that.denom)
  )

  def +(that: Rational) = this.add(that)

  def neg = new Rational(-number, denom)

  def unary_- = neg

  def sub(that: Rational) = this + -that

  def -(that: Rational) = sub(that)

  def less(that: Rational) = number * that.denom < that.number * denom

  def <(that: Rational) = less(that)

  def max(that: Rational) = if (this < that) that else this

  override def toString: String = number + "/" + denom
}