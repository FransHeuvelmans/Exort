import org.scalatest._

class InSortTest extends FlatSpec with Matchers {
  "Unsorted LongRow table after sorting" should "be sorted" in {
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

  "unsorted StringRow table after sorting" should "be sorted" in {
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
}
