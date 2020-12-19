package com.nrinaudo.pbt.scalacheck

import org.scalacheck.Properties
import org.scalacheck.Prop.{forAll, propBoolean}

object SampleProperties extends Properties("demo") {

  property("booleans") = forAll { (b1: Boolean, b2: Boolean, f: Boolean => Int) =>
    f(b1) == f(b2)
  }

  property("ints") = forAll { (i1: Int, i2: Int, f: Int => Boolean) =>
    f(i1) == f(i2)
  }

  property("strings") = forAll { (f: String => Boolean) =>
    f("Scala") == f("Haskell")
  }

  property("options") = forAll { (o1: Option[Int], o2: Option[Int], f: Option[Int] => Boolean) =>
    f(o1) == f(o2)
  }

  property("lists") = forAll { (l1: List[Int], l2: List[Int], f: List[Int] => Boolean) =>
    f(l1) == f(l2)
  }

  property("product types") = forAll { (p1: ProductType, p2: ProductType, f: ProductType => Boolean) =>
    f(p1) == f(p2)
  }

  property("sum types") = forAll { (s1: SumType, s2: SumType, f: SumType => Boolean) =>
    f(s1) == f(s2)
  }

  property("recursive types") = forAll { (t1: Tree[Int], t2: Tree[Int], f: Tree[Int] => Boolean) =>
    f(t1) == f(t2)
  }

}
