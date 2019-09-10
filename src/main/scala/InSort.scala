import scala.collection.LinearSeq
import scala.concurrent.Future
import scala.util.Sorting

sealed trait  SortableRow {
  def getContent(): Array[String]
}
case class LongRow(v: Long, content: Array[String]) extends SortableRow {
  def getContent() = this.content
}
case class StringRow(v: String, content: Array[String]) extends SortableRow {
  def getContent() = this.content
}

// TODO: Focus on the memory use of the different Sorting methods

object InSort {

  def SortLongRow(dataset: List[LongRow], reverse: Boolean = false): Array[LongRow] = {
    val fixedData = dataset.toArray
    if (reverse) {
      Sorting.stableSort(fixedData, (a: LongRow, b: LongRow) => a.v > b.v)
    } else {
      Sorting.stableSort(fixedData, (a: LongRow, b: LongRow) => a.v < b.v)
    }
    fixedData
  }

  def SortStringRow(dataset: List[StringRow], reverse: Boolean = false):  List[StringRow] = {
    if (reverse) {
      dataset.sortBy((in: StringRow) => in.v)(math.Ordering.String.reverse)
    } else {
      dataset.sortBy((in: StringRow) => in.v)
    }
  }

  /**
   * TODO: Might add a multicore-sort but this has to be performance tested
   * Think using java's Timsort (with a copy) or a scala implementation for each core
   * and then do a in-memory mergesort for those elements
   */

}
