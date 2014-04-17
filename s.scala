import scala.language.higherKinds
import scala.language.implicitConversions

trait CT1v0 extends UnsafeTraversals {
  trait Fresh extends Iterator[Name => Name] {
    def next(default: Name): Name

    def next: Name => Name = x => next(x)
    def hasNext: Boolean = true
  }

  sealed trait Bind[Var, Idx <: PatternIndex, Scope] {
    def head: Pattern
    def body: Scope
  }

  private case class Bindata[Var, Idx <: PatternIndex, Scope]
    (head: Pattern, body: Scope) extends Bind[Var, Idx, Scope]

  trait PatternIndex {
    def depth: Int // how deep from its binder
    def index: Int // pointer into pattern names
  }

  object Bind {
    def apply[Var, Idx <: PatternIndex, Scope]
      (head: Pattern, body: Scope)
      (implicit mkVar: Name => Var, mkIdx: (Int, Int) => Idx):
        Bind[Var, Idx, Scope] = Bindata(head, bind[Var, Idx, Scope](head.names, body))

    def bind[Var, Idx <: PatternIndex, Scope]
      (head: Vector[Name], body: Scope)
      (implicit mkVar: Name => Var, mkIdx: (Int, Int) => Idx) =
    {
      val vars = head map mkVar
      def bindAt(depth: Int)(body: Any): Any = {
        val index = vars indexOf body
        if (index >= 0)
          mkIdx(depth, index)
        else body match {
          // no need to test for binders unless there are naked names in body: Scope.
          // case Bindata(head, body) =>
          //   Bindata(head, bindAt(depth + 1)(body))

          case body: Product =>
            unsafeCopy(body, body.productIterator.map(bindAt(depth + 1)))

          case _ =>
            body
        }
      }
      bindAt(0)(body).asInstanceOf[Scope]
    }
  }

  trait Pattern {
    this: Product =>
    def names: Vector[Name] =
      Vector(unsafeMapReduce[Iterator[Name]]({
        case name: Name => Iterator(name)
      })(_ ++ _)(this).toSeq: _*)
  }

  trait Name extends Pattern {
    this: Product =>
    def iterator: Iterator[Name] = Iterator(this) ++ new NameIterator(this)
    def next: Name = IndexedName(this, 0)
    override def names: Vector[Name] = Vector(this)
  }

  class NameIterator(var current: Name) extends Iterator[Name] {
    def hasNext: Boolean = true
    def next: Name = { current = current.next ; current }
  }

  case class SymbolicName(symbol: Symbol) extends Name {
    override def toString = symbol.toString
  }

  case class IntegralName(integer: Int) extends Name {
    override def toString = s"@$integer"
    override def next: Name = IntegralName(integer + 1)
  }

  case class IndexedName(root: Name, index: Int) extends Name {
    override def toString = s"$root$index"
    override def next: Name = IndexedName(root, index + 1)
  }

  implicit def symbolToName(symbol: Symbol): Name = SymbolicName(symbol)
  implicit def nameToTVar[T <% Name](name: T): TVar = TVar(name)
  implicit def indicesToTIdx(depth: Int, index: Int): TIdx = TIdx(depth, index)

  trait Type
  case class TIdx(depth: Int, index: Int) extends Type with PatternIndex
  case class TVar(name: Name) extends Type
  case class Arrow(domain: Type, range: Type) extends Type
  case class All(get: Bind[TVar, TIdx, Type]) extends Type
  case class BAll(lower: Iterable[Type], upper: Iterable[Type], get: Bind[TVar, TIdx, Type])

  object All {
    def apply(head: Name, body: Type): All = All(Bind(head, body))
  }

  implicit def nameToVar[T <% Name](name: T): Var = Var(name)
  implicit def indicesToIdx(depth: Int, index: Int): Idx = Idx(depth, index)

  trait Term
  case class Var(name: Name) extends Term
  case class Idx(depth: Int, index: Int) extends Term with PatternIndex

  case class AnnotatedAbs(argType: Type, get: Bind[Var, Idx, Term]) extends Term
  case class TAbs(get: Bind[TVar, TIdx, Term]) extends Term
}

trait Pretty extends CT1v0 {
/*
  case class Avoider(toAvoid: Set[Name]) extends Fresh {
    def next(default: Name): Name = default.iterator.filterNot(toAvoid contains _).next
    def avoid(name: Name): Avoider = copy(toAvoid = toAvoid + name)
  }

  object Avoider {
    def empty: Avoider = Avoider(Set.empty)
  }

  def pretty(typ: Type)(implicit avoider: Avoider = Avoider.empty): String = typ match {
    case TVar(name) =>
      name.toString

    case Arrow(domain, range) =>
      s"(${pretty(domain)} -> ${pretty(range)})"

    case All(a, body) =>
      s"(∀${a.name}. ${pretty(body)(avoider avoid a.name)})"

    case BAll(a, lower, upper, body) =>
      val low = lower map plain mkString " ∪ "
      val high = upper map plain mkString " ∩ "
      s"(∀${a.name} ∈ [$low, $high]. ${plain(body)(avoider avoid a.name)})"
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

    case BAll(a, lower, upper, body) =>
      val low = lower map plain mkString " ∪ "
      val high = upper map plain mkString " ∩ "
      s"(∀${a.name} ∈ [$low, $high]. ${plain(body)})"
  }

  def parsimonious(typ: Type): String = {
    implicit def avoider: Avoider = Avoider(typ.fv.map(_.name))
    typ match {
      case TVar(name) =>
        name.toString

      case Arrow(domain, range) =>
        s"(${parsimonious(domain)} -> ${parsimonious(range)})"

      case All(a, body) =>
        s"(∀${a.name}. ${parsimonious(body)})"

      case BAll(a, lower, upper, body) =>
        val low = lower map parsimonious mkString " ∪ "
        val high = upper map parsimonious mkString " ∩ "
        s"(∀${a.name} ∈ [$low, $high]. ${parsimonious(body)})"
    }
  }*/
}

object Test extends App with Pretty {
  val t = TAbs(Bind('a, AnnotatedAbs(Arrow('a, All(Bind('a, 'a))), Bind('x, 'x))))
  println(t)
/*
  // note that indices are depths, not de-Bruijn indices.
  // it is so that we don't have to distinguish binders at runtime in ADTs.
  val shadowy = All('a, Arrow('a, All('a, Arrow(All('a, TIdx(2)), All('a, TIdx(4))))))

  val bounded = All('b, BAll('a, Vector('L1, Arrow('b, 'b)), Vector('U1, Arrow('Int, 'Int)), Arrow('a, 'a)))

  println(pretty(idT))
  println(plain(idT))
  println(parsimonious(idT))

  println()

  println(pretty(shadowy))
  println(plain(shadowy))
  println(parsimonious(shadowy))

  println()

  println(pretty(bounded))
  println(plain(bounded))
  println(parsimonious(bounded))
 */
}
