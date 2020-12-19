package com.nrinaudo.pbt

sealed trait Fun[A, C] extends (A => C) {
  def pfun: A :=> C
  def defaultValue: C
  def total: A => C

  /** Shrinks this Fun by both attempting to reduce the underlying function's domain and the value for cut out parts
    * of the domain.
    */
  def shrink(shrinkC: C => Stream[C]): Stream[Fun[A, C]] = {
    // Shrinks the domain of the tested function.
    val shrunkFunction = pfun
      .shrink(shrinkC)
      .map(p => Fun.Partial(p, defaultValue))

    // Shrinks the default value.
    val shrunkDefaultValue = shrinkC(defaultValue)
      .map(c => Fun.Partial(pfun, c))

    // We want to shrink the function's domain first, as this will result in the largest cuts in the test space.
    shrunkFunction #::: shrunkDefaultValue
  }

  override def apply(a: A) = total(a)
}

object Fun {

  /** Fun that's based on a total function.
    *
    * The main goal of splitting this and `Partial` is that we don't want to attempt to print a total function using
    * it's partial projection. Imagine trying to print a total function whose domain is Int.
    */
  final case class Total[A: Argument, C](defaultValue: C, total: A => C) extends Fun[A, C] {
    lazy val pfun         = Argument[A].build(total)
    override def toString = "<function>"
  }

  /** Fun that's based on a partial function.
    *
    * This is the result of shrinking a `Total` and should not be created directly.
    */
  final case class Partial[A, C](pfun: A :=> C, defaultValue: C) extends Fun[A, C] {
    override val total = a => pfun.toPartial(a).getOrElse(defaultValue)

    override def toString =
      "{" + pfun.table.map { case (in, out) => s"$in => $out" }.mkString(", ") + s", _ => $defaultValue}"
  }
}
