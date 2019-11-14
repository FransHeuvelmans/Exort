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


  "unsorted dev.hillman.Exort.VaryRow table after sorting" should "be sorted" in {
    val sortOrder = Tools.sortKeyType.stringKeyType :: Tools.sortKeyType.integerKeyType :: Tools.sortKeyType.decimalKeyType :: Nil
    val unsorted =
      VaryRow("c" :: Nil, 5.0 :: Nil, 11 :: Nil, sortOrder, Array("c", "11", "5.0")) ::
        VaryRow("d" :: Nil, 9.0 :: Nil, 13 :: Nil, sortOrder, Array("d", "13", "9.0")) ::
        VaryRow("c" :: Nil, 4.0 :: Nil, 11 :: Nil, sortOrder, Array("c", "11", "4.0")) ::
        VaryRow("e" :: Nil, 3.5 :: Nil, 15 :: Nil, sortOrder, Array("e", "15", "3.5")) ::
        VaryRow("b" :: Nil, 2.0 :: Nil, 6 :: Nil, sortOrder, Array("b", "6", "2.0")) ::
        VaryRow("a" :: Nil, 3.0 :: Nil, 2 :: Nil, sortOrder, Array("a", "2", "3.0")) ::
        VaryRow("c" :: Nil, 3.0 :: Nil, 9 :: Nil, sortOrder, Array("c", "9", "3.0")) ::
        VaryRow("a" :: Nil, 1.0 :: Nil, 1 :: Nil, sortOrder, Array("a", "1", "1.0")) :: Nil
    val out = InSort.sortVaryRow(unsorted)
    out(0).content(1) === "1"
    out(1).content(1) === "2"
    out(2).content(0) === "b"
    out(3).content(0) === "c"
    out(3).content(1) === "9"
    out(4).content(1) === "11"
    out(4).content(2) === "4.0"
    out(5).content(2) === "5.0"
    out(5).content(0) === "d"
    out(6).content(0) === "e"
  }
}
