
// Class 2.5
object rationals {

  class Rational(x: Int, y: Int) {
    def number = x

    def denom = y

    def add(that: Rational) = new Rational(
      number * that.denom + that.number * denom
      , (denom * that.denom)
    )

    def neg = new Rational(-number, denom)

    def sub(that: Rational) = add(that.neg)

    override def toString: String = number + "/" + denom
  }

  def addRattional(r: Rational, s: Rational): Unit = {
    new Rational(
      r.number * s.denom + s.number * r.denom
      , (s.denom * r.denom)
    )
  }

  def makeString(r: Rational) = r.number + "/" + r.denom
}

val rational = new rationals.Rational(1, 2)
rational.number
rational.denom

val x = new rationals.Rational(1, 3)
val y = new rationals.Rational(5, 7)
val z = new rationals.Rational(3, 2)
x.number
x.denom
x.sub(y).sub(z)