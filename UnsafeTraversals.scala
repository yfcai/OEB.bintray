// cf.
// https://github.com/Blaisorblade/scrap-expr-boilerplate/blob/master/src/traversal/Reflection.scala
// https://github.com/Blaisorblade/scrap-expr-boilerplate/blob/master/src/traversal/RewritingSYB.scala

trait UnsafeTraversals {
  // whatever mappable are mapped
  // the rest are reduced
  def unsafeMapReduce[R]
    (map: PartialFunction[Any, R])
    (reduce: (R, R) => R)
    (tree: Any): R =
    map.asInstanceOf[PartialFunction[Any, R]].applyOrElse[Any, R](tree, {
      case tree: Product =>
        tree.productIterator.map(unsafeMapReduce(map)(reduce)).reduce(reduce)
    })

  def unsafeCopy[T <: Product](t: T, args: Iterator[Any]): T = {
    val klass = t.getClass
    // DO NOT call args.length! args is an iterator and can only iterate once!
    if (t.productArity == 0 && args.isEmpty)
      t
    else {
      val copyMethods = klass.getMethods.filter(_.getName == "copy")
      copyMethods.length match {
        case 0 => sys error s"unsafeCopy: $klass has no copy method"
        case 1 => ()
        case n => sys error s"unsafeCopy: $klass has $n copy methods, but it should have only 1"
      }
      val copy = copyMethods.head
      val aseq = args.toSeq.asInstanceOf[Seq[Object]]
      if (t.productArity != aseq.length)
        sys error s"unsafeCopy: $klass has product arity ${t.productArity}, but ${aseq.length} arguments are given"
      val params = copy.getParameterTypes
      val result: Any =
        if (params.length == 1 && params.head.isInstance(aseq)) // scala variadic
          copy.invoke(t, aseq)
        else if (params.length == aseq.length) // difference between boxed & unboxed things...
          copy.invoke(t, aseq: _*)
        else
          sys error s"unsafeCopy: $klass.copy has parameter types ${params.toList}, but arguments types are ${aseq.map(_.getClass)}"
      result.asInstanceOf[T]
    }
  }

  def unsafeRewrite[T](rule: PartialFunction[Nothing, Any])(tree: T): T =
    rule.asInstanceOf[PartialFunction[T, T]].applyOrElse[T, T](tree, {
      case tree: T with Product =>
        unsafeCopy(tree, tree.productIterator.map(unsafeRewrite(rule)))

      case _ =>
        tree
    })

  def typeMatches(classes: Array[Class[_]], args: Seq[Object]): Boolean = {
    if (classes.isEmpty && args.isEmpty)
      true
    else if (classes.length == args.length)
      classes.zip(args).map(p => p._1 isInstance p._2).min
    else // classes.length != args.length
      false
  }
}
