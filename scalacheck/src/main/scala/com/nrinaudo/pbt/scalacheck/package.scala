package com.nrinaudo.pbt

import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}

package object scalacheck {

  implicit def arbFunction[A: Cogen: Argument, C: Arbitrary]: Arbitrary[A => C] = Arbitrary {
    for {
      total <- Gen.function1[A, C](Arbitrary.arbitrary[C])
      c     <- Arbitrary.arbitrary[C]
    } yield Fun.Total(c, total)
  }

  implicit def shrinkFunction[A, C](implicit shrinkC: Shrink[C]): Shrink[A => C] = Shrink {
    case f: Fun[A, C] => f.shrink(shrinkC.shrink)
    case _            => Stream.empty

  }

}
