// cf.
// https://github.com/Blaisorblade/scrap-expr-boilerplate/blob/master/src/traversal/Reflection.scala
// https://github.com/Blaisorblade/scrap-expr-boilerplate/blob/master/src/traversal/RewritingSYB.scala

trait UnsafeTraversals {
  // whatever mappable are mapped
  // the rest are reduced
  def unsafeMapReduce[R]
    (map: PartialFunction[Nothing, R])
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
    else
      klass.getMethods.find(_.getName == "copy").
        getOrElse(sys error s"No `copy` method found when reflectively copying $t of type $klass").
        invoke(t, args.toSeq.asInstanceOf[Seq[Object]]: _*).asInstanceOf[T]
  }

  def unsafeRewrite[T](rule: PartialFunction[Nothing, Any])(tree: T): T =
    rule.asInstanceOf[PartialFunction[T, T]].applyOrElse[T, T](tree, {
      case tree: T with Product =>
        unsafeCopy(tree, tree.productIterator.map(unsafeRewrite(rule)))

      case _ =>
        tree
    })
}
