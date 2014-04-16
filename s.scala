@convert
trait PatternFunctors {
  //fixed Point
  trait List[T]

  //variants	
  case class Nil[T]() extends List[T]
  case class Cons[T](head:T, tail:List[T]) extends List[T]

  // other things
  object List {
    def apply[T](xs: T*): List[T] = xs.foldRight(Nil[T]())(Cons.apply[T])
  }
}

object Hello extends App with PatternFunctors {

  def sum(xs: List[Int]): Int = xs.fold[Int] {
    case Nil() => 0
    case Cons(x, y) => x + y
  }

  val xs = List(1, 2, 3, 4)

  println(s"sum($xs) = ${sum(xs)}")
}
