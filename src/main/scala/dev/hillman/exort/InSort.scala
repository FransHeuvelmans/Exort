package dev.hillman.exort

import scala.reflect._
import scala.util.Sorting



sealed trait SortableRow {
  def getContent: Array[String]
}
case class LongRow(v: Long, content: Array[String]) extends SortableRow {
  def getContent: Array[String] = this.content
}
case class DoubleRow(v: Double, content: Array[String]) extends SortableRow {
  def getContent: Array[String] = this.content
}
case class StringRow(v: String, content: Array[String]) extends SortableRow {
  def getContent: Array[String] = this.content
}
case class VaryRow(v1: List[String],
                   v2: List[Double],
                   v3: List[Long],
                   vs: Array[Tools.sortKeyType.sortKeyType],
                   content: Array[String])
    extends SortableRow {
  def getContent: Array[String] = this.content
}
case class VaryRowComplex(v1: List[String],
                          v2: List[BigDecimal],
                          v3: List[BigInt],
                          vs: Array[Tools.sortKeyType.sortKeyType],
                          content: Array[String])
    extends SortableRow {
  def getContent: Array[String] = this.content
}

object LongRowOrdering extends Ordering[LongRow] {
  override def compare(x: LongRow, y: LongRow): Int =
    Ordering.Long.compare(x.v, y.v)
}
object DoubleRowOrdering extends Ordering[DoubleRow] {
  override def compare(x: DoubleRow, y: DoubleRow): Int =
    Ordering.Double.TotalOrdering.compare(x.v, y.v)
}
object StringRowOrdering extends Ordering[StringRow] {
  override def compare(x: StringRow, y: StringRow): Int =
    Ordering.String.compare(x.v, y.v)
}

object InSort {

  implicit val longRowOrdering = LongRowOrdering
  implicit val doubleRowOrdering = DoubleRowOrdering
  implicit val stringRowOrdering = StringRowOrdering
  implicit val varyRowOrdering = VaryRowOrdering

  def sortLongRow(dataset: List[LongRow],
                  reverse: Boolean = false): Array[LongRow] = {
    val ctag = classTag[LongRow]
    if (reverse) {
      Sorting.stableSort(dataset)(ctag, LongRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)
    }
  }

  def sortDoubleRow(dataset: List[DoubleRow],
                    reverse: Boolean = false): Array[DoubleRow] = {
    val ctag = classTag[DoubleRow]
    if (reverse) {
      Sorting.stableSort(dataset)(ctag, DoubleRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)
    }
  }

  def sortStringRow(dataset: List[StringRow],
                    reverse: Boolean = false): Array[StringRow] = {
    val ctag = classTag[StringRow]
    if (reverse) {
      Sorting.stableSort(dataset)(ctag, StringRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)
    }
  }

  def sortVaryRow(dataset: List[VaryRow],
                  reverse: Boolean = false): Array[VaryRow] = {
    val ctag = classTag[VaryRow]
    if (reverse) {
      // This reverse only works when reversing the whole ordering (not usable for individual columns)
      Sorting.stableSort(dataset)(ctag, VaryRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)(ctag, VaryRowOrdering)
    }
  }

  def sortVaryRowComplex(dataset: List[VaryRowComplex],
                         reverse: Boolean = false): Array[VaryRowComplex] = {
    val ctag = classTag[VaryRowComplex]
    if (reverse) {
      // This reverse only works when reversing the whole ordering (not usable for individual columns)
      Sorting.stableSort(dataset)(ctag, ComplexVROrdering.reverse)
    } else {
      Sorting.stableSort(dataset)(ctag, ComplexVROrdering)
    }
  }
}
