package week4

trait Expr {
  // Classification

  def isNumber: Boolean

  def isSum: Boolean

  // Accessor
  def numValue: Int

  def leftOp: Expr

  def rightOp: Expr

  def eval(e: Expr): Int = {
    if (e.isNumber) e.numValue
    else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
    else throw new Error("Unknown expression" + e)
  }

}

class Number(n: Int) extends Expr {
  // Classification

  def isNumber: Boolean = true

  def isSum: Boolean = false

  // Accessor
  def numValue: Int = n

  def leftOp: Expr = throw new Error("Number.leftOp")

  def rightOp: Expr = throw new Error("Number.rightOp")

}

class Sum(e1: Expr, e2: Expr) extends Expr {
  // Classification

  def isNumber: Boolean = false

  def isSum: Boolean = true

  // Accessor
  def numValue: Int = throw new Error("Sum.numValue")

  def leftOp: Expr = e1

  def rightOp: Expr = e2

}

