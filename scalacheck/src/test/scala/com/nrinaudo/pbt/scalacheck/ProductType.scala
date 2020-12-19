package com.nrinaudo.pbt.scalacheck

import com.nrinaudo.pbt.Argument
import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}

/** Dummy product type to show of how `Argument` can be used for them. */
case class ProductType(i: Int, s: String, b: Boolean)

object ProductType {

  // - Argument instance -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  // Our product type is equivalent to a `Tuple2[Int, String]`, for which we have an `Argument` instance.
  implicit val argument: Argument[ProductType] = {
    val to: ProductType => (Int, (String, Boolean)) = p => (p.i, (p.s, p.b))

    val from: ((Int, (String, Boolean))) => ProductType = { case (i, (s, b)) => ProductType(i, s, b) }

    Argument.buildMap(to, from)
  }

  // - Usual scalacheck boilerplate ------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Note that I'm using a very small subset of the space of strings here - not because of any technical limitation,
  // but because ScalaCheck's default instance is horrible for printing purposes, and the entire point of this demo
  // is to print things.

  implicit val arbitrary: Arbitrary[ProductType] = Arbitrary {
    for {
      i <- Arbitrary.arbitrary[Int]
      s <- Gen.oneOf("Scala", "Haskell")
      b <- Arbitrary.arbitrary[Boolean]
    } yield ProductType(i, s, b)
  }

  implicit val cogen: Cogen[ProductType] =
    Cogen.tuple3[Int, String, Boolean].contramap(p => (p.i, p.s, p.b))

  implicit val shrink: Shrink[ProductType] = Shrink { p =>
    val shrinkI = Shrink.shrink(p.i).map { i =>
      p.copy(i = i)
    }

    val shrinkS = Stream.empty

    val shrinkB = Shrink.shrink(p.b).map { b =>
      p.copy(b = b)
    }

    shrinkI #::: shrinkS #::: shrinkB
  }
}
