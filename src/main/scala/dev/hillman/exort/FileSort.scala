package dev.hillman.exort

import java.io.File

import scala.util.Sorting

sealed trait TempSortedFile {

  // For now cast down to a list of doubles
  def file: File
}

case class LongSortedFile(vlow: Long, vhigh: Long, file: File)
    extends TempSortedFile {
  require(vlow <= vhigh)
  def distance(other: TempSortedFile,
                        reverse: Boolean = false): Long = {
    val dis = other match {
      case o: LongSortedFile => {
        if (this.vlow > o.vhigh) { // case 2 & 3
          this.vlow - o.vhigh // - / r+
        } else if (this.vhigh < o.vlow) { // case 1 & 4
          this.vhigh - o.vlow // + / r-
        } else {
          0L
        }
      }
      case _ => 0L
    }
    if (reverse) {
      -dis
    } else {
      dis
    }
  }
}

case class DoubleSortedFile(vlow: Double, vhigh: Double, file: File)
    extends TempSortedFile {
  require(vlow <= vhigh)
  def distance(other: TempSortedFile,
                        reverse: Boolean = false): Double = {
    val dis = other match {
      case o: LongSortedFile => {
        if (this.vlow > o.vhigh) {
          this.vlow - o.vhigh
        } else if (this.vhigh < o.vlow) {
          this.vhigh - o.vlow
        } else {
          0.0
        }
      }
      case _ => 0.0
    }
    if (reverse) {
      -dis
    } else {
      dis
    }
  }
}

case class StringSortedFile(vlow: String, vhigh: String, file: File)
    extends TempSortedFile {
  require(vlow <= vhigh)
  def distance(other: TempSortedFile,
                        reverse: Boolean = false): List[Int] = {
    val dis = other match {
      case o: StringSortedFile => {
        if (this.vlow > o.vhigh) {
          Tools.StringDistanceLowerCase(this.vlow, o.vhigh)
        } else if (this.vhigh < o.vlow) {
          Tools.StringDistanceLowerCase(this.vhigh, o.vlow)
        } else {
          0 :: Nil
        }
      }
      case _ => 0 :: Nil
    }
    if (reverse) {
      dis.map(-1 * _)
      // TODO: Reverse ordering in this case could be more complicated that simply reversing the values
      // would you want cc c bb b aa a OR c cc b bb a aa
    } else {
      dis
    }
  }
}

/* Note because VaryRow only has one ordering (with many possible suborderings)
 * only the start and end of a ordered file are tracked (not whether they are
 * big or small) */
case class VarySortedFile(vstart: VaryRow, vend: VaryRow, file: File)
    extends TempSortedFile {

  def distance(other: TempSortedFile,
                        reverse: Boolean = false): List[Double] = {
    require(!reverse) // Should use special ordering types for varyrows
    other match {
      case o: VarySortedFile => {
        if (VaryRowOrdering.gt(this.vstart, o.vend)) {
          VaryRowOrdering.distance(this.vstart, o.vend, Nil)
        } else if (VaryRowOrdering.lt(this.vend, o.vstart)) {
          VaryRowOrdering.distance(this.vend, o.vstart, Nil)
        } else {
          0.0 :: Nil
        }
      }
      case _ => 0.0 :: Nil
    }
  }
}

object FileSort {
  // Distance comparisons less than
  def lt[T](a: List[T], b: List[T])(implicit num: Numeric[T]): Boolean = {
    val maxSize = Math.max(a.length, b.length)
    val aFull = a.padTo(maxSize, num.zero)
    val bFull = b.padTo(maxSize, num.zero)
    val comparisons = aFull.zip(bFull).map(ab => num.minus(ab._1, ab._2))
    comparisons.foreach(c =>
      if (num.gt(c, num.zero)) {
        return false
      } else if (num.lt(c, num.zero)) {
        return true
    })
    false
  }

  def gt[T](a: List[T], b: List[T])(implicit num: Numeric[T]): Boolean = {
    val maxSize = Math.max(a.length, b.length)
    val aFull = a.padTo(maxSize, num.zero)
    val bFull = b.padTo(maxSize, num.zero)
    val comparisons = aFull.zip(bFull).map(ab => num.minus(ab._1, ab._2))
    comparisons.foreach(c =>
      if (num.lt(c, num.zero)) {
        return false
      } else if (num.gt(c, num.zero)) {
        return true
    })
    false
  }

  def sortDistances(distances: List[(List[Double], Int)]): Array[(List[Double], Int)] =
    Sorting.stableSort(distances, (a, b) => lt(a._1, b._1))

}
