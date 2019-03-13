package week4

trait Expr {
  def show: String

  def simplify: Expr

  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }

}

case class Number(n: Int) extends Expr {

  def show: String = n + ""

  def simplify: Expr = this

}

case class Sum(e1: Expr, e2: Expr) extends Expr {

  def show: String = e1.show + "+" + e2.show

  def simplify: Expr = this
}

