package com.nrinaudo.pbt.scalacheck

import com.nrinaudo.pbt.Argument
import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}

/** Dummy sum type to show of how `Argument` can be used for them. */
sealed abstract class SumType extends Product with Serializable

object SumType {
  case object Branch1 extends SumType
  case object Branch2 extends SumType
  case object Branch3 extends SumType

  // - Argument instance -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  implicit val argument: Argument[SumType] = {
    val to: SumType => Either[Unit, Either[Unit, Unit]] = {
      case Branch1 => Left(())
      case Branch2 => Right(Left(()))
      case Branch3 => Right(Right(()))
    }

    val from: Either[Unit, Either[Unit, Unit]] => SumType = {
      case Left(())         => Branch1
      case Right(Left(()))  => Branch2
      case Right(Right(())) => Branch3
    }

    Argument.buildMap(to, from)
  }

  // - Usual scalacheck boilerplate ------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val arbitrary: Arbitrary[SumType] = Arbitrary(Gen.oneOf(Branch1, Branch2, Branch3))

  implicit val cogen: Cogen[SumType] =
    Cogen[Int].contramap {
      case Branch1 => 1
      case Branch2 => 2
      case Branch3 => 3
    }

  implicit val shrink: Shrink[SumType] = Shrink {
    case Branch1 => Stream.empty
    case Branch2 => Stream(Branch1)
    case Branch3 => Stream(Branch2, Branch1)
  }
}
