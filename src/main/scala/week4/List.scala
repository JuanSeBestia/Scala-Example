package week4

trait List[T] {
  def isEmpty: Boolean

  def head: T

  def tail: List[T]
  def nth(index: Int): T =
    if (index < 0) throw new IndexOutOfBoundsException()
    else if (index == 0) head
    else tail.nth(index - 1)
}

class Const[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.head")
}

