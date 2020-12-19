package com.nrinaudo.pbt.scalacheck

import com.nrinaudo.pbt.Argument
import org.scalacheck.{Arbitrary, Cogen, Gen, Shrink}

/** Basic binary tree implementation used to show that "complex", recursive types are no problem. */
sealed abstract class Tree[+A] extends Product with Serializable

object Tree {
  final case class Node[A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]
  final case object Leaf                                            extends Tree[Nothing]

  // - Argument instance -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  implicit def argument[A: Argument]: Argument[Tree[A]] = {
    val to: Tree[A] => Option[(Tree[A], (A, Tree[A]))] = {
      case Leaf                     => None
      case Node(left, value, right) => Some((left, (value, right)))
    }

    val from: Option[(Tree[A], (A, Tree[A]))] => Tree[A] = {
      case None                         => Leaf
      case Some((left, (value, right))) => Node(left, value, right)
    }

    Argument.buildMap(to, from)
  }

  // - Usual scalacheck boilerplate ------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------

  val genLeaf: Gen[Leaf.type] = Gen.const(Leaf)

  def genNode[A: Arbitrary](depth: Int): Gen[Node[A]] =
    for {
      left  <- genTree[A](depth - 1)
      value <- Arbitrary.arbitrary[A]
      right <- genTree[A](depth - 1)
    } yield Node(left, value, right)

  def genTree[A: Arbitrary](depth: Int): Gen[Tree[A]] =
    if(depth <= 0) genLeaf
    else Gen.oneOf(genLeaf, genNode[A](depth))

  implicit def arbitrary[A: Arbitrary]: Arbitrary[Tree[A]] = Arbitrary(genTree[A](4))

  implicit def cogen[A: Cogen]: Cogen[Tree[A]] = Cogen[List[A]].contramap { (tree: Tree[A]) =>
    @annotation.tailrec
    def toList(trees: List[Tree[A]], curr: List[A]): List[A] = trees match {
      case Leaf :: tail                     => toList(tail, curr)
      case Node(left, value, right) :: tail => toList(left :: right :: tail, value :: curr)
      case Nil                              => curr
    }

    toList(List(tree), Nil)
  }

  implicit def shrink[A: Shrink]: Shrink[Tree[A]] = Shrink {
    case Leaf => Stream.empty
    case node @ Node(left, value, right) =>
      Stream(left, right) #::: Shrink.shrink(left).map(l => node.copy(left = l)) #::: Shrink
        .shrink(right)
        .map(r => node.copy(right = r)) #::: Shrink.shrink(value).map(v => node.copy(value = v))
  }
}
