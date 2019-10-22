package dev.hillman.exort

import org.scalatest._

class InSortTest extends FlatSpec with Matchers {
  "Unsorted dev.hillman.Exort.LongRow table after sorting" should "be sorted" in {
    val unsorted = List(LongRow(1, Array("aa", "bb", "c", "d")),
      LongRow(10, Array("c", "bb", "c", "d")),
      LongRow(2, Array("b", "bb", "c", "d")),
      LongRow(9, Array("e", "bb", "c", "d")),
      LongRow(4, Array("d", "bb", "c", "d"))
    )
    val out = InSort.sortLongRow(unsorted)
    out(0).v === 1
    out(1).v === 2
    out(2).content(0) === "d"
    out(3).content(0) === "e"
    out(4).v === 10
  }

  "unsorted dev.hillman.Exort.StringRow table after sorting" should "be sorted" in {
    val unsorted = List(StringRow("z", Array("aa", "bb", "c", "d")),
      StringRow("v", Array("c", "bb", "c", "d")),
      StringRow("b", Array("b", "bb", "c", "d")),
      StringRow("a", Array("e", "bb", "c", "d")),
      StringRow("x", Array("d", "bb", "c", "d"))
    )
    val out = InSort.sortStringRow(unsorted)
    out(0).v === "a"
    out(1).v === "b"
    out(2).content(0) === "c"
    out(3).content(0) === "d"
    out(4).v === "z"
  }

  "unsorted dev.hillman.Exort.DoubleRow table after sorting" should "be sorted" in {
    val unsorted = List(DoubleRow(1.0, Array("x", "y", "1.0", "z")),
      DoubleRow(0.5, Array("x", "y", "0.5", "z")),
      DoubleRow(20.1, Array("x", "y", "20.1", "z")),
      DoubleRow(-0.1, Array("x", "y", "-0.1", "z")),
      DoubleRow(0.1, Array("x", "y", "-0.1", "z"))
    )
    val out = InSort.sortDoubleRow(unsorted)
    out(0).v === -0.1
    out(2).v === 0.5
    out.last.v === 20.1

    val reverseOut = InSort.sortDoubleRow(unsorted, true)
    reverseOut(0).v === 20.1
    reverseOut(2).v === 0.5
    reverseOut.last.v === -0.1
  }
}
