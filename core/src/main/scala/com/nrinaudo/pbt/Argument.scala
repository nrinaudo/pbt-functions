package com.nrinaudo.pbt

/** Type class for types that can be used as the domain of a `PFun`.
  *
  * We provided instances of `Argument` for the basic building blocks. Most use cases should be a combination of
  * existing implementations, most likely through `buildMap`.
  */
trait Argument[A] { self =>
  def build[C](f: A => C): A :=> C
}

object Argument {

  /** Summons an implicit `Argument[A]`. */
  def apply[A](implicit arg: Argument[A]): Argument[A] = arg

  /** Allows you to easily provide an `Argument[A]` if you already have an `Argument[B]`.
    *
    * Note that the way this is built, you need `to` to be total - there must be a way to go from your source type to
    * your target type. You do not need `from` to be total, but it *does* need to be defined for the range of `to`.
    */
  def buildMap[A, B](to: A => B, from: B => A)(implicit argB: => Argument[B]): Argument[A] = new Argument[A] {
    override def build[C](f: A => C) = PFun.imap(to, from, argB.build(from andThen f))
  }

  /** `Argument` instance of `Unit`. */
  implicit val argUnit: Argument[Unit] = new Argument[Unit] {
    override def build[C](f: Unit => C) = PFun.unit(f(()))
  }

  implicit def argPair[A, B](implicit argA: => Argument[A], argB: => Argument[B]): Argument[(A, B)] =
    new Argument[(A, B)] {
      override def build[C](f: ((A, B)) => C) =
        PFun.product(
          argA.build(a => argB.build(b => f((a, b))))
        )
    }

  implicit def argEither[A, B](implicit argA: => Argument[A], argB: => Argument[B]): Argument[Either[A, B]] =
    new Argument[Either[A, B]] {
      override def build[C](f: Either[A, B] => C) = PFun.union(
        PFun.left(argA.build(a => f(Left(a)))),
        PFun.right(argB.build(b => f(Right(b))))
      )
    }

  /** `Argument` instance for `Boolean`.
    *
    * This is where the work on `argUnit` and `argEither` come together: `Boolean` can be considered to be a different
    * representation of `Either[Unit, Unit]`, with:
    *  - `false` mapping to `Right(())`
    *  - `true` mapping to `Left(())`
    *
    * Give that we have an `Argument[Either[Unit, Unit]]` and functions to go from one to the other and back, we can
    * just rely on the existing instances.
    */
  implicit val argBool: Argument[Boolean] = {
    val to: Boolean => Either[Unit, Unit] = {
      case true  => Left(())
      case false => Right(())
    }

    val from: Either[Unit, Unit] => Boolean = {
      case Left(())  => true
      case Right(()) => false
    }

    buildMap(to, from)
  }

  /** `Argument` instance for `Option`.
    *
    * This follows the same logic as `Boolean`. An `Option[A]` is equivalent to an `Either[Unit, A]`, and we have
    * an `Argument` instance for that.
    */
  implicit def argOption[A: Argument]: Argument[Option[A]] = {
    val to: Option[A] => Either[Unit, A] = {
      case Some(a) => Right(a)
      case None    => Left(())
    }

    val from: Either[Unit, A] => Option[A] = {
      case Right(a) => Some(a)
      case Left(()) => None
    }

    buildMap(to, from)
  }

  /** `Argument` instance for `List`.
    *
    * This follows the same logic as all of our "complex" types: a `List[A]` is equivalent to an `Option[(A, List[A])]`
    *  - a list is an optional head and tail. We have an `Argument[Option[(A, List[A])]]`, so we're good to go.
    *
    * Note, however, that this `Argument` instance is recursive: we have an `Argument[List[A]]` because we can simplify
    * a `List` to an `A` and a `List[A]`. We need an `Argument[List[A]]` in order to provide an `Argument[List[A]]`.
    *
    * If this makes you feel of structural recursion, you're not the only one, and I intend to explore this further.
    */
  implicit def argList[A: Argument]: Argument[List[A]] = {
    val to: List[A] => Option[(A, List[A])] = {
      case Nil          => None
      case head :: tail => Some((head, tail))
    }

    val from: Option[(A, List[A])] => List[A] = {
      case None               => Nil
      case Some((head, tail)) => head :: tail
    }

    buildMap(to, from)
  }

  /** `Argument` instance for `Int`.
    *
    * This works by turning an `Int` into its two's complement representation.
    */
  implicit def argInt: Argument[Int] = {

    def div2(i: Int): Int =
      math.floor(i / 2D).toInt

    val to: Int => Either[(Boolean, Int), Boolean] = {
      case 0  => Right(false)
      case -1 => Right(true)
      case i => {
        Left((i % 2 != 0, div2(i)))
      }
    }

    val from: Either[(Boolean, Int), Boolean] => Int = {
      case Right(true)  => -1
      case Right(false) => 0
      case Left((b, i)) => 2 * i + (if(b) 1 else 0)
    }

    buildMap(to, from)
  }

  /** `Argument` instance for `String`.
    *
    * This one is far more straightforward than the rest. A `String` is a list of char codes, for which we already
    * have an `Argument`.
    */
  implicit def argString: Argument[String] = {
    val to: String => List[Int] = _.map(_.toInt).toList

    val from: List[Int] => String = _.map(_.toChar).mkString

    buildMap(to, from)
  }
}
