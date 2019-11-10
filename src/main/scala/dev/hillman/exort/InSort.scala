package dev.hillman.exort


import scala.reflect._
import scala.util.Sorting

sealed trait SortableRow {
  def getContent: Array[String]
}
case class LongRow(v: Long, content: Array[String]) extends SortableRow {
  def getContent(): Array[String] = this.content
}
case class DoubleRow(v: Double, content: Array[String]) extends SortableRow  {
  def getContent(): Array[String] = this.content
}
case class StringRow(v: String, content: Array[String]) extends SortableRow {
  def getContent(): Array[String] = this.content
}
case class VaryRow(v1: List[String],
                   v2: List[BigDecimal],
                   v3: List[BigInt],
                   vs:List[Tools.sortKeyType.sortKeyType],
                   content: Array[String]) extends SortableRow {
  def getContent(): Array[String] = this.content
}

object LongRowOrdering extends Ordering[LongRow] {
  override def compare(x: LongRow, y: LongRow): Int = Ordering.Long.compare(x.v, y.v)
}
object DoubleRowOrdering extends Ordering[DoubleRow] {
  override def compare(x: DoubleRow, y: DoubleRow): Int = Ordering.Double.IeeeOrdering.compare(x.v, y.v)
}
object StringRowOrdering extends Ordering[StringRow] {
  override def compare(x: StringRow, y: StringRow): Int = Ordering.String.compare(x.v, y.v)
}
object VaryRowOrdering extends Ordering[VaryRow] {
  @scala.annotation.tailrec
  override def compare(x: VaryRow, y: VaryRow): Int = x.vs.head match {
    case Tools.sortKeyType.stringKeyType => {
      val cmp = Ordering.String.compare(x.v1.head, y.v1.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRow(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
      val newThat = VaryRow(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
      VaryRowOrdering.compare(newThis, newThat)
    }
    case Tools.sortKeyType.decimalKeyType => {
      val cmp = Ordering.BigDecimal.compare(x.v2.head, y.v2.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRow(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
      val newThat = VaryRow(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
      VaryRowOrdering.compare(newThis, newThat)
    }
    case Tools.sortKeyType.integerKeyType => {
      val cmp = Ordering.BigInt.compare(x.v3.head, y.v3.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRow(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
      val newThat = VaryRow(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
      VaryRowOrdering.compare(newThis, newThat)
    }
  }

  @scala.annotation.tailrec
  def distance(x: VaryRow, y: VaryRow): Double = x.vs.head match {
    case Tools.sortKeyType.stringKeyType => {
      val cmp = Tools.StringDistance(x.v1.head, y.v1.head)
      if (cmp != 0.0) {
        return cmp
      }
      val newThis = VaryRow(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
      val newThat = VaryRow(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
      VaryRowOrdering.distance(newThis, newThat)
    }
    case Tools.sortKeyType.decimalKeyType => {
      val cmp = y.v2.head - x.v2.head
      if (cmp.abs > 0.001) {
        return cmp.toDouble  // TODO: Can go horribly wrong
      }
      val newThis = VaryRow(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
      val newThat = VaryRow(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
      VaryRowOrdering.distance(newThis, newThat)
    }
    case Tools.sortKeyType.integerKeyType => {
      val cmp = y.v3.head - x.v3.head
      if (cmp != 0) {
        return cmp.toDouble  // TODO: Can go horribly wrong
      }
      val newThis = VaryRow(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
      val newThat = VaryRow(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
      VaryRowOrdering.distance(newThis, newThat)
    }
  }
}

object InSort {

  implicit val longRowOrdering = LongRowOrdering
  implicit val doubleRowOrdering = DoubleRowOrdering
  implicit val stringRowOrdering = StringRowOrdering
  implicit val varyRowOrdering = VaryRowOrdering

  def sortLongRow(dataset: List[LongRow], reverse: Boolean = false): Array[LongRow] = {
    val ctag = classTag[LongRow]
    if (reverse) {
      Sorting.stableSort(dataset)(ctag, LongRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)
    }
  }

  def sortDoubleRow(dataset: List[DoubleRow], reverse: Boolean = false): Array[DoubleRow] = {
    val ctag = classTag[DoubleRow]
    if (reverse) {
      Sorting.stableSort(dataset)(ctag, DoubleRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)
    }
  }

  def sortStringRow(dataset: List[StringRow], reverse: Boolean = false):  Array[StringRow] = {
    val ctag = classTag[StringRow]
    if (reverse) {
      Sorting.stableSort(dataset)(ctag, StringRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)
    }
  }

  def sortVaryRow(dataset: List[VaryRow], reverse: Boolean = false): Array[VaryRow] = {
    val ctag = classTag[VaryRow]
    if (reverse) {
      Sorting.stableSort(dataset)(ctag, VaryRowOrdering.reverse)
    } else {
      Sorting.stableSort(dataset)
    }
  }
}
