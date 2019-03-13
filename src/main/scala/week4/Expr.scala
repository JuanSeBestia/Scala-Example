package week4

trait Expr {
  def show: String = this match {
    case Number(n) => n + ""
    case Sum(e1, e2) => e1.show + "+" + e2.show
    case Prod(Sum(s1, s2), Sum(s3, s4)) => "(" + Sum(s1, s2).show + ")" + "*" + "(" + Sum(s3, s4).show + ")"
    case Prod(Sum(s1, s2), e2) => "(" + Sum(s1, s2).show + ")" + "*" + e2.show
    case Prod(e1, Sum(s1, s2)) => e1.show + "*" + "(" + Sum(s1, s2).show + ")"
    case Prod(e1, e2) => e1.show + "*" + e2.show
    case Var(name) => name
  }

  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }

}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

case class Prod(e1: Expr, e2: Expr) extends Expr

case class Var(name: String) extends Expr

