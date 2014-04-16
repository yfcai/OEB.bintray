import scala.language.higherKinds
import scala.language.implicitConversions

trait CT1v0 {
  trait Monad[M[_]] {
    def unit[A](a: A): M[A]
    def bind[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  // fresh-name type class
  abstract class Fresh[M[_]](implicit val monadInstance: Monad[M]) {
    def fresh(default: Name): M[Name]
  }

  implicit class MonadComprehension[M[_], A](ma: M[A])(implicit instance: Monad[M]) {
    import instance._
    def map[B](f: A => B): M[B] = bind(ma)(a => unit(f(a)))
    def flatMap[B](f: A => M[B]): M[B] = bind(ma)(f)
  }

  trait Name {
    def iterator: Iterator[Name] = Iterator(this) ++ new NameIterator(this)
    def next: Name = IndexedName(this, 0)
  }

  class NameIterator(var current: Name) extends Iterator[Name] {
    def hasNext: Boolean = true
    def next: Name = { current = current.next ; current }
  }

  case class SymbolicName(symbol: Symbol) extends Name {
    override def toString = symbol.toString.tail
  }

  case class IntegralName(integer: Int) extends Name {
    override def toString = s"@$integer"
    override def next: Name = IntegralName(integer + 1)
  }

  case class IndexedName(root: Name, index: Int) extends Name {
    override def toString = s"$root$index"
    override def next: Name = IndexedName(root, index + 1)
  }

  trait ADT[T] {
    def children: Iterator[T]
    def map(f: T => T): T
  }

  trait Binder[Scope <: ADT[Scope], Var <: Scope, Index <: Scope] {
    def mkVar(name: Name): Var
    def mkIndex(i: Int): Index

    // never test for binder
    // increment depth at every constructor
    // starting from 0: \x. x becomes Abs(Var('x), Idx(0))
    def bind(head: Var, body: Scope): Scope = bindAt(0, head)(body)

    def bindAt(index: Int, head: Var)(body: Scope): Scope =
      if (head == body)
        mkIndex(index)
      else
        body map bindAt(index + 1, head)

    def unbind(name: Name, body: Scope): Scope = unbindAt(0, name)(body)

    def unbindAt(index: Int, name: Name)(body: Scope): Scope =
      if (mkIndex(index) == body)
        mkVar(name)
      else
        body map unbindAt(index + 1, name)
  }

  trait TypeBinder extends Binder[Type, TVar, TIdx] {
    def mkVar(name: Name): TVar = TVar(name)
    def mkIndex(i: Int): TIdx = TIdx(i)
  }

  trait Type extends ADT[Type] {
    def children: Iterator[Type]

    // free variables must override this
    lazy val fv: Set[TVar] = children.foldRight(Set.empty[TVar])(_.fv ++ _)
  }

  case class TIdx(index: Int) extends Type {
    def children: Iterator[Type] = Iterator.empty
    def map(f: Type => Type): Type = this
  }

  case class TVar(name: Name) extends Type {
    def children: Iterator[Type] = Iterator.empty
    def map(f: Type => Type): Type = this
    override lazy val fv = Set(this)
  }

  case class Arrow(domain: Type, range: Type) extends Type {
    def children: Iterator[Type] = Iterator(domain, range)
    def map(f: Type => Type): Type = Arrow(f(domain), f(range))
  }

  private case class Universal(head: TVar, body: Type) extends Type {
    def children: Iterator[Type] = Iterator(body)
    def map(f: Type => Type): Type = Universal(head, f(body))
  }

  private case class BoundedU(head: TVar, lower: Vector[Type], upper: Vector[Type], body: Type) extends Type {
    def children: Iterator[Type] = Iterator(body) ++ lower.iterator ++ upper.iterator
    def map(f: Type => Type): Type = BoundedU(head, lower map f, upper map f, f(body))
  }

  object All extends TypeBinder  {
    def apply(head: TVar, body: Type): Type = Universal(head, bind(head, body))

    def unapply[M[_]](typ: Type)(implicit instance: Fresh[M]): Option[M[(TVar, Type)]] =
      typ match {
        case Universal(head, body) =>
          import instance._
          val m: M[(TVar, Type)] = for {
            name <- new MonadComprehension(fresh(head.name))(monadInstance)
          } yield (mkVar(name), unbind(name, body))
          Some(m)

        case _ =>
          None
      }
  }

  implicit def symbolToTVar(symbol: Symbol): TVar = TVar(SymbolicName(symbol))
}

trait Pretty extends CT1v0 {

  // reader monad: generates shadowing-avoiding names

  case class FreshReader[T](run: Set[Name] => T)

  object FreshReader {
    def unit[A](a: A): FreshReader[A] = FreshReader(_ => a)
  }

  implicit object FreshReaderMonad extends Monad[FreshReader] {
    def unit[A](a: A): FreshReader[A] = FreshReader.unit(a)
    def bind[A, B](ma: FreshReader[A])(f: A => FreshReader[B]): FreshReader[B] =
      FreshReader(avoid => f(ma.run(avoid)).run(avoid))
  }

  object FreshReaderFresh extends Fresh[FreshReader] {
    def fresh(default: Name): FreshReader[Name] = freshReader(default)
  }

  def freshReader(default: Name): FreshReader[Name] =
    FreshReader(avoid => default.iterator.find(name => ! avoid(name)).get)

  def avoidThis[T](name: Name, freshM: FreshReader[T]): FreshReader[T] =
    FreshReader(avoid => freshM.run(avoid + name))

  // identity monad: generates global names

  case class NewType[T](get: T)

  implicit object NewTypeMonad extends Monad[NewType] {
    def unit[A](a: A): NewType[A] = NewType(a)
    def bind[A, B](ma: NewType[A])(f: A => NewType[B]): NewType[B] = f(ma.get)
  }

  object NewTypeFresh extends Fresh[NewType] {
    var counter = -1
    def fresh(default: Name): NewType[Name] = {
      counter += 1
      NewType(IntegralName(counter))
    }
  }

  def prettyM(typ: Type): FreshReader[String] = {
    implicit val freshness = FreshReaderFresh
    typ match {
      case TVar(name) =>
        FreshReader.unit(name.toString)

      case Arrow(domain, range) =>
        for {
          sigma <- prettyM(domain)
          tau <- prettyM(range)
        } yield s"($sigma -> $tau)"

      case All(m) =>
        for {
          data <- m
          (alpha, body) = data // if we connect the 2 occurrences of `data`, then get "filter is not a member"
          tau <- avoidThis(alpha.name, prettyM(body))
        } yield s"(∀${alpha.name}. $tau)"
    }
  }

  def pretty(typ: Type): String = prettyM(typ) run Set.empty

  def uglyM(typ: Type): NewType[String] = {
    implicit val freshness = NewTypeFresh
    typ match {
      case TVar(name) =>
        NewType(name.toString)

      case Arrow(domain, range) =>
        for {
          sigma <- uglyM(domain)
          tau <- uglyM(range)
        } yield s"($sigma -> $tau)"

      case All(m) =>
        for {
          data <- m
          (alpha, body) = data // if we connect the 2 occurrences of `data`, then get "filter is not a member"
          tau <- uglyM(body)
        } yield s"(∀${alpha.name}. $tau)"
    }
  }

  def ugly(typ: Type): String = uglyM(typ).get
}

object Test extends App with Pretty {
  val idT = All('a, Arrow('a, All('a, Arrow(All('a, 'a), All('a, 'a)))))

  println(pretty(idT))
  println(ugly(idT))
}
