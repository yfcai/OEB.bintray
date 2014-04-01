@convert
trait PatternFunctors {
  //fixed Point
  trait List[T]

  //variants	
  case class Nil[T]() extends List[T]
  case class Cons[T](head:T, tail:List[T]) extends List[T]
}

object Hello extends App with PatternFunctors {

  /* [error] value fold is not a member of Hello.List[Int]

  def sum(xs: List[Int]): Int = xs.fold[Int] {
    case Nil() => 0
    case Cons(x, y) => x + y
  }
  */

  println("Goodbye cruel world!")
}