# Shrinking and showing functions

## Introduction

This is a Scala implementation of the technique described in [Shrinking and showing functions](https://dl.acm.org/doi/abs/10.1145/2364506.2364516).

Not only is it really quite fun, but it's also very useful: when generating random functions in a PBT framework, being able to shrink them and show the minimal function required to reproduce an issue can be a huge time saver.

## Using it

Well, you can't, not really. This is not meant for production (yet?). There's a lot of polishing up to do, and a lot more standard instances to provide - the current implementation doesn't work with any other function type than `Function1`, for instance. And I'm pretty sure shapeless could be used to automate all the work for ADTs.

But if you want to play with it, you can clone the project and toy with `scalacheck` module, which plugs in to ScalaCheck and should immediately let you shrink and show your arbitrary `Function1`s.

## How it works

The concept is truly elegant (it's alright for me to say that given I was in no way part of its design). Limiting ourselves to functions that take ADTs as input, we provide a way of turning a total function into an aggregate of smaller, partial ones that each work on a component of the ADT.

For example, functions whose domain is the sum of `A` and `B` and whose range is `C` will be split into:
- `A => C`
- `B => C`
- some glue to discriminate between the two and take their union

This ends up being the same function as our original one, but one whose domain we can take large swathes out of in one go. We could, for example, drop the `B => C` part.

The first benefit of this approach is that it makes it very easy to shrink functions: we can reduce their domain little by little until we end up with only the bits that cause a particular test case to fail.

The second benefit is that, by shrinking the domain down, we go from a total function with a potentially infinite domain to a total function with the smallest possible domain needed to reproduce the issue - and we can then use a default value to fake the same domain as the original function.

There are, of course, lots of fun details that this explanation doesn't cover - recursive data types, for instance, are quite cleverly encoded (although that encoding was a bit of a pain to implement in Scala because of stack safety issues). The code does its best to be self-documenting and clear, if you want to dig deeper. Or you could read the original paper.

## Samples

You can look at the `SampleProperties.scala` file for a series of examples.

Here's a teaser:

```scala
// This is clearly false - there *must* an arbitrary function that doesn't evaluate
// to the same result for arbitrary inputs.
val propStupid = forAll { (l1: List[Int], l2: List[Int], f: List[Int] => Boolean) =>
    f(l1) == f(l2)
  }
```

Evaluating this property will result in:
```
[info] > ARG_0: List("-1")
[info] > ARG_1: List()
[info] > ARG_2: {List(-1) => true, _ => false}
[info] > ARG_0_ORIGINAL: List("57988037", "0")
[info] > ARG_1_ORIGINAL: List("2147483647", "-678993779")
[info] > ARG_2_ORIGINAL: <function>
```

You can see that we found a very good minimal reproduction test case:
- `List(-1)`
- the empty list
- a function that maps `List(-1)` to `true` and any other input to `false`
