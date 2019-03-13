package week4

trait List[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]

  def prepend[U >: T](elem: U): List[U] = new Const[U](elem, this)

  def nth(index: Int): T =
    if (index < 0) throw new IndexOutOfBoundsException()
    else if (index == 0) head
    else tail.nth(index - 1)
}

class Const[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

object Nil extends List[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}

object List {
  // List(1,2) == List.apply(1,2)
  def apply[T](x1: T, x2: T): List[T] = new Const[T](x1, new Const[T](x2, Nil))

  def apply[T](): List[T] = new Nil[T]
}

object test {
  val x: List[String] = Nil
}