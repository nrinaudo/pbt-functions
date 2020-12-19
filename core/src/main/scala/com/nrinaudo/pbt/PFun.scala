package com.nrinaudo.pbt

/** Recursive representation of a partial function.
  *
  * This recursive representation is achieved by considering that elements of the function's domain must be ADTs:
  * potentially recursive sums of product types. We provide the basic building block to support these:
  * - `unit`, for the unit type.
  * - `product`, for the product of two types.
  * - `union`, `left` and `right` for sum types: `union` for the union, `left` and `right` for the discriminator.
  * - `void` and a combination of the above for recursive types.
  *
  * Additionally, `map` allows easy creation of `PFun` for isomorphic types.
  *
  * See `Argument` for concrete examples of using these.
  *
  * `PFun` allows us to easily prune entire chunks of its definition in order to shrink it. For example, if you have
  * a function that is defined as either an `f` or a `g`, you can reduce its domain by limiting it to just `f`, or
  * just `g`.
  *
  * This allows us to gradually reduce the domain of a total function to only keep the bits that cause a test to fail.
  * This should be a finite subset and, hopefully, a reasonably small one.
  */
trait PFun[A, C] {

  /** Returns the entire definition of the function as a list of mappings from domain to range. */
  def table: List[(A, C)]

  /** Turns the function in a potentially infinite list of successively smaller functions. */
  def shrink(shrinkC: C => Stream[C]): Stream[A :=> C]

  /** Turns this "virtual" representation of a concrete function into a concrete one. */
  def toPartial: A => Option[C]
}

object PFun {

  /** Function that returns the specified constant for `()`.
    *
    * This is useful to represent, for example, the `None` subdomain of a function of domain `Option[A]`.
    */
  def unit[C](value: C): Unit :=> C = new (Unit :=> C) {
    override def table = List(((), value))

    // Shrinks the domain to nothing, or shrinks the output value.
    override def shrink(shrinkC: C => Stream[C]) = {
      val head: Unit :=> C         = void()
      val tail: Stream[Unit :=> C] = shrinkC(value).map(unit)

      head #:: tail
    }

    override def toPartial = _ => Some(value)
  }

  /** Function whose domain is the simplest possible product type, `A * B`.
    *
    * This can be used to represent more complex product types by nesting: `(A, (B, (C, D)))`, for example.
    */
  def product[A, B, C](fun: => (A :=> (B :=> C))): Tuple2[A, B] :=> C = new (Tuple2[A, B] :=> C) {
    override def table =
      for {
        (a, bc) <- fun.table
        (b, c)  <- bc.table
      } yield ((a, b), c)

    override def shrink(shrinkC: C => Stream[C]) =
      fun.shrink(bc => bc.shrink(shrinkC)).map(abc => product(abc))

    override def toPartial = {
      case (a, b) =>
        for {
          b2 <- fun.toPartial(a)
          c  <- b2.toPartial(b)
        } yield c
    }
  }

  /** Function whose domain is strictly the left branch of an `Either[A, B]`.
    *
    * This is useful to add discrimination to union types (created through `union`).
    */
  def left[A, B, C](fun: => A :=> C): Either[A, B] :=> C = new (Either[A, B] :=> C) {
    override def table =
      for {
        (a, c) <- fun.table
      } yield (Left(a), c)

    override def shrink(shrinkC: C => Stream[C]) =
      fun.shrink(shrinkC).map(a => left(a))

    override def toPartial = {
      case Left(a)  => fun.toPartial(a)
      case Right(_) => None
    }
  }

  /** Function whose domain is strictly the right branch of an `Either[A, B]`.
    *
    * This is useful to add discriminatation to union types (created through `union`).
    */
  def right[A, B, C](fun: => B :=> C): Either[A, B] :=> C = new (Either[A, B] :=> C) {
    override def table =
      for {
        (b, c) <- fun.table
      } yield (Right(b), c)

    override def shrink(shrinkC: C => Stream[C]) =
      fun.shrink(shrinkC).map(b => right(b))

    override def toPartial = {
      case Left(_)  => None
      case Right(b) => fun.toPartial(b)
    }
  }

  /** Function whose domain is the union of the domains of the two specified functions.
    *
    * Combined with `left` and `right`, this allows us to represent sum types (discriminated union types).
    */
  def union[A, C](fun1: A :=> C, fun2: A :=> C): A :=> C = new (A :=> C) {
    override def table = fun1.table ++ fun2.table

    // Shrinks first to `fun1` and `fun2` to try and take out the largest possible amount of the domain.
    // Then shrinks `fun1` and `fun2` themselves.
    override def shrink(shrinkC: C => Stream[C]) =
      Stream(fun1, fun2) #::: fun1.shrink(shrinkC) #::: fun2.shrink(shrinkC)

    override def toPartial = a => fun1.toPartial(a).orElse(fun2.toPartial(a))
  }

  /** Function without a domain. */
  def void[A, C](): A :=> C = new (A :=> C) {
    override def table                           = List.empty
    override def shrink(shrinkC: C => Stream[C]) = Stream.empty
    override def toPartial                       = _ => None
  }

  /** Creates a function from an existing one and a mapping back and force to another type.
    *
    * This is useful for representing types in terms of more "basic" ones. For examples, `Boolean` can be seen
    * as `Either[Unit, Unit]`, which we can easily represent through `unit`, `left`, `right` and `union`.
    */
  def imap[A, B, C](to: A => B, from: B => A, fun: => B :=> C): A :=> C = new (A :=> C) {
    override def table =
      for {
        (b, c) <- fun.table
      } yield (from(b), c)

    override def shrink(shrinkC: C => Stream[C]) = fun.shrink(shrinkC).map { f =>
      imap(to, from, f)
    }

    override def toPartial = a => fun.toPartial(to(a))
  }
}
