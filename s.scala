import scala.language.higherKinds
import scala.language.implicitConversions

trait CT1v0 extends UnsafeTraversals {
  ct1v0 =>

  trait Fresh extends Iterator[Name => Name] {
    def next(default: Name): Name

    def next: Name => Name = x => next(x)
    def hasNext: Boolean = true
  }

  trait Pattern {
    this: Product =>
    def names: Vector[Name] =
      Vector(unsafeMapReduce[Iterator[Name]]({
        case name: Name => Iterator(name)
      })(_ ++ _)(this).toSeq: _*)

    def rename(_newNames: Seq[Name]): Pattern = {
      require(names.length == _newNames.length)
      val newNames = _newNames.iterator // use mutation to thread newNames in preorder
      def loop(any: Any): Any = any match {
        case name: Name =>
          newNames.next

        case product: Pattern with Product =>
          unsafeCopy(product, product.productIterator.map(loop))
      }
      loop(this).asInstanceOf[Pattern]
    }
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
    private[this] def outer: AnyRef = ct1v0 // work around https://issues.scala-lang.org/browse/SI-4440
  }

  // TODO idea: make mkVar, mkIdx into a type class

  object Bind {
    def apply[Var, Idx <: PatternIndex, Scope]
      (head: Pattern, body: Scope)
      (implicit mkVar: Name => Var, mkIdx: (Int, Int) => Idx):
        Bind[Var, Idx, Scope] = Bindata(head, bind[Var, Idx, Scope](head.names, body))

    def unapply[Var, Idx <: PatternIndex, Scope]
      (bind: Bind[Var, Idx, Scope])
      (implicit mkVar: Name => Var, mkIdx: (Int, Int) => Idx,
        fresh: Fresh): Option[(Pattern, Scope)] =
      bind match {
        case Bindata(pattern, body) =>
          val newNames = pattern.names.map(x => fresh.next(x))
          Some((pattern rename newNames, unbind[Var, Idx, Scope](newNames, body)))

        case _ =>
          None
      }

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

    def unbind[Var, Idx <: PatternIndex, Scope]
      (head: Vector[Name], body: Scope)
      (implicit mkVar: Name => Var, mkIdx: (Int, Int) => Idx): Scope =
    {
      def unbindAt(depth: Int)(body: Any): Any = body match {
        case idx: PatternIndex if idx.depth == depth && idx == mkIdx(depth, idx.index) =>
          mkVar(head(idx.index))

        case body: Product =>
          unsafeCopy(body, body.productIterator.map(unbindAt(depth + 1)))

        case _ =>
          body
      }
      unbindAt(0)(body).asInstanceOf[Scope]
    }
  }

  // scala collections cannot be a part of patterns because
  // 1. `unsafeCopy` requires a `copy` method defined automatically for case classes,
  // 2. scala collections don't have this `copy` method defined in a sensible manner.

  case class Names(_names: Name*) extends Pattern with Product {
    override lazy val names: Vector[Name] = _names.toVector
    override def productArity: Int = names.length
    override def productIterator: Iterator[Any] = names.iterator

    def copy(_names: Name*): Names = Names(_names: _*)
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

  trait FreeNames {
    lazy val fv: Set[Name] = unsafeMapReduce[Set[Name]]({
      case index: PatternIndex => Set.empty
      case name: Name => Set(name)
      case Bindata(_, body: FreeNames) => body.fv
      case otherwise if ! otherwise.isInstanceOf[Product] => Set.empty
    })(_ ++ _)(this)
  }

  implicit def symbolToName(symbol: Symbol): Name = SymbolicName(symbol)
  implicit def nameToTVar[T <% Name](name: T): TVar = TVar(name)
  implicit def indicesToTIdx(depth: Int, index: Int): TIdx = TIdx(depth, index)

  trait Type extends FreeNames
  case class TIdx(depth: Int, index: Int) extends Type with PatternIndex
  case class TVar(name: Name) extends Type
  case class Arrow(domain: Type, range: Type) extends Type
  case class All(get: Bind[TVar, TIdx, Type]) extends Type
  case class BAll(lower: Iterable[Type], upper: Iterable[Type], get: Bind[TVar, TIdx, Type]) extends Type

  object All {
    def apply(head: Pattern, body: Type): All = All(Bind(head, body))
  }

  implicit def nameToVar[T <% Name](name: T): Var = Var(name)
  implicit def indicesToIdx(depth: Int, index: Int): Idx = Idx(depth, index)

  trait Term extends FreeNames
  case class Var(name: Name) extends Term
  case class Idx(depth: Int, index: Int) extends Term with PatternIndex

  case class AnnotatedAbs(argType: Type, get: Bind[Var, Idx, Term]) extends Term
  case class TAbs(get: Bind[TVar, TIdx, Term]) extends Term
}

trait Pretty extends CT1v0 {

  case class Avoider(toAvoid: Set[Name]) extends Fresh {
    def next(default: Name): Name = default.iterator.filterNot(toAvoid contains _).next
    def avoid(pattern: Pattern): Avoider = copy(toAvoid = toAvoid ++ pattern.names)
  }

  object Avoider {
    def empty: Avoider = Avoider(Set.empty)
  }

  def pretty(typ: Type)(implicit avoider: Avoider = Avoider.empty): String = typ match {
    case TVar(name) =>
      name.toString

    case Arrow(domain, range) =>
      s"(${pretty(domain)} -> ${pretty(range)})"

    case All(Bind(a, body)) =>
      s"(∀$a. ${pretty(body)(avoider avoid a)})"

    case BAll(lower, upper, Bind(a, body)) =>
      val low = lower map pretty mkString " ∪ "
      val high = upper map pretty mkString " ∩ "
      s"(∀$a ∈ [$low, $high]. ${pretty(body)(avoider avoid a)})"
  }

  case class Counter(var i: Int = -1) extends Fresh {
    def next(default: Name): Name = { i += 1 ; IntegralName(i) }
  }

  def plain(typ: Type)(implicit counter: Fresh = Counter()): String = typ match {
    case TVar(name) =>
      name.toString

    case Arrow(domain, range) =>
      s"(${plain(domain)} -> ${plain(range)})"

    case All(Bind(a, body)) =>
      s"(∀$a. ${plain(body)})"

    case BAll(lower, upper, Bind(a, body)) =>
      val low = lower map plain mkString " ∪ "
      val high = upper map plain mkString " ∩ "
      s"(∀$a ∈ [$low, $high]. ${plain(body)})"
  }

  def parsimonious(typ: Type): String = {
    implicit def avoider: Avoider = Avoider(typ.fv)
    typ match {
      case TVar(name) =>
        name.toString

      case Arrow(domain, range) =>
        s"(${parsimonious(domain)} -> ${parsimonious(range)})"

      case All(Bind(a, body)) =>
        s"(∀$a. ${parsimonious(body)})"

      case BAll(lower, upper, Bind(a, body)) =>
        val low = lower map parsimonious mkString " ∪ "
        val high = upper map parsimonious mkString " ∩ "
        s"(∀a ∈ [$low, $high]. ${parsimonious(body)})"
    }
  }
}

object Test extends App with Pretty {
  val term = TAbs(Bind('a, AnnotatedAbs(Arrow('a, All(Bind('a, 'a))), Bind('x, 'x))))

  // note that indices are depths, not de-Bruijn indices.
  // it is so that we don't have to distinguish binders at runtime in ADTs.
  val shadowy = All(Bind('a, Arrow('a, All(Bind('a, Arrow(All(Bind('a, TIdx(3, 0))), All(Bind('a, TIdx(6, 0)))))))))

  val bounded = All('b, BAll(Vector('L1, Arrow('b, 'b)), Vector('U1, Arrow('Int, 'Int)), Bind('a, Arrow('a, 'a))))


  println(pretty(shadowy))
  println(plain(shadowy))
  println(parsimonious(shadowy))

  println()

  println(pretty(bounded))
  println(plain(bounded))
  println(parsimonious(bounded))

  println()

  println(s"('a → 'b → 'c).fv = ${Arrow('a, Arrow('b, 'c)).fv}")
  println(s"(∀'a. 'a → 'b).fv = ${All(Bind(Names('a), Arrow('a, 'b))).fv}")
}
