import scala.language.higherKinds
import scala.language.implicitConversions

trait CT1v0 {
  trait Fresh extends Iterator[Name => Name] {
    def next(default: Name): Name

    def next: Name => Name = x => next(x)
    def hasNext: Boolean = true
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

    def unapply[M[_]](typ: Type)(implicit fresh: Fresh): Option[(TVar, Type)] =
      typ match {
        case Universal(head, body) =>
          val name = fresh next head.name
          val tau = unbind(name, body)
          Some((mkVar(name), tau))

        case _ =>
          None
      }
  }

  implicit def symbolToTVar(symbol: Symbol): TVar = TVar(SymbolicName(symbol))
}

trait Pretty extends CT1v0 {

  case class Avoider(toAvoid: Set[Name]) extends Fresh {
    def next(default: Name): Name = default.iterator.filterNot(toAvoid contains _).next
    def avoid(name: Name): Avoider = copy(toAvoid = toAvoid + name)
  }

  def pretty(typ: Type)(implicit avoider: Avoider = Avoider(Set.empty)): String = typ match {
    case TVar(name) =>
      name.toString

    case Arrow(domain, range) =>
      s"(${pretty(domain)} -> ${pretty(range)})"

    case All(a, body) =>
      s"(∀${a.name}. ${pretty(body)(avoider avoid a.name)})"
  }

  case class Counter(var i: Int = -1) extends Fresh {
    def next(default: Name): Name = { i += 1 ; IntegralName(i) }
  }

  def plain(typ: Type)(implicit counter: Fresh = Counter()): String = typ match {
    case TVar(name) =>
      name.toString

    case Arrow(domain, range) =>
      s"(${plain(domain)} -> ${plain(range)})"

    case All(a, body) =>
      s"(∀${a.name}. ${plain(body)})"
  }
}

object Test extends App with Pretty {
  val idT = All('a, Arrow('a, All('a, Arrow(All('a, 'a), All('a, 'a)))))

  println(pretty(idT))
  println(plain(idT))
}
