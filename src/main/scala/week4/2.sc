import week4._

val x = Sum(Number(2), Number(2))
val y = new Sum(new Number(2), new Number(2))
x.show
x.eval + 2
Prod(Number(2), Number(2))
Var("y")
Sum(Prod(Number(2), Var("x")), Var("y")).show
Prod(Sum(Number(2), Var("x")), Var("y")).show