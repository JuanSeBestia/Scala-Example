import week4._


def singleton[T](element: T) = new Const[T](element, new Nil[T]())

singleton(1)
singleton(true)

def nth[T](index: Int, xs: List[T]): T =
  if (index < 0) throw new IndexOutOfBoundsException()
  else if (index == 0) xs.head
  else nth(index - 1, xs.tail)

val list = new Const(1, new Const(2, new Const(3, new Nil())))

list.nth(1)

nth(1,list)