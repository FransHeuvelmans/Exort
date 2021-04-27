package dev.hillman.exort

import org.scalatest.flatspec.AnyFlatSpec

class ComplexVROrderingTest extends AnyFlatSpec {
  "Equal ComplexVRows" should "be comparible" in {
    val a = VaryRowComplex("a" :: Nil, Nil, Nil, Array(Tools.sortKeyType.stringKeyType), Array())
    val b = VaryRowComplex("a":: Nil, Nil, Nil, Array(Tools.sortKeyType.stringKeyType), Array())

    assert(ComplexVROrdering.compare(a, b) === 0)
  }
  "Different ComplexVRows" should "compare correctly" in {
    val a = VaryRowComplex("A" :: Nil, Nil, Nil, Array(Tools.sortKeyType.stringKeyType), Array())
    val b = VaryRowComplex("a":: Nil, Nil, Nil, Array(Tools.sortKeyType.stringKeyType), Array())

    assert(ComplexVROrdering.compare(a, b) < 0)
  }
}
