package dev.hillman.exort

import java.io.File

import scala.util.Sorting

sealed trait TempSortedFile {
  // Distance between two sorted files (if there is any)
  def distance(other: TempSortedFile, reverse: Boolean = false): List[Double]

  // For now cast down to a list of doubles
  def file: File
}

case class LongSortedFile(vlow: Long, vhigh: Long, file: File)
    extends TempSortedFile {
  require(vlow < vhigh)
  override def distance(other: TempSortedFile,
                        reverse: Boolean = false): List[Double] = {
    val dis = other match {
      case o: LongSortedFile => {
        if (this.vlow > o.vhigh) { // case 2 & 3
          (this.vlow - o.vhigh).toDouble // - / r+
        } else if (this.vhigh < o.vlow) { // case 1 & 4
          (this.vhigh -o.vlow).toDouble // + / r-
        } else {
          0.0
        }
      }
      case _ => 0.0
    }
    if (reverse) {
      -dis :: Nil
    } else {
      dis :: Nil
    }
  }
}

case class DoubleSortedFile(vlow: Double, vhigh: Double, file: File)
    extends TempSortedFile {
  require(vlow < vhigh)
  override def distance(other: TempSortedFile,
                        reverse: Boolean = false): List[Double] = {
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
      -dis :: Nil
    } else {
      dis :: Nil
    }
  }
}

case class StringSortedFile(vlow: String, vhigh: String, file: File)
    extends TempSortedFile {
  require(vlow <= vhigh)
  override def distance(other: TempSortedFile,
                        reverse: Boolean = false): List[Double] = {
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
      dis.map(-_.toDouble)
      // TODO: Reverse ordering in this case could be more complicated that simply reversing the values
      // would you want cc c bb b aa a OR c cc b bb a aa
    } else {
      dis.map(_.toDouble)
    }
  }
}

case class VarySortedFile(vlow: VaryRow, vhigh: VaryRow, file: File)
    extends TempSortedFile {

  // require(VaryRowOrdering.lt(vlow, vhigh))  Need to check ordering only because VaryRows have build-in reverse

  override def distance(other: TempSortedFile,
                        reverse: Boolean = false): List[Double] = {
    require(!reverse) // Should use special ordering types for varyrows
    other match {
      case o: VarySortedFile => {
        if (VaryRowOrdering.gt(this.vlow, o.vhigh)) {
          VaryRowOrdering.distance(this.vlow, o.vhigh, Nil)
        } else if (VaryRowOrdering.lt(this.vhigh, o.vlow)) {
          VaryRowOrdering.distance(this.vhigh, o.vlow, Nil)
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
  def lt(a: List[Double], b: List[Double]): Boolean = {
    val maxSize = Math.max(a.length, b.length)
    val aFull = a.padTo(maxSize, 0.0)
    val bFull = b.padTo(maxSize, 0.0)
    val comparisons = aFull.zip(bFull).map(ab => ab._1 - ab._2)
    comparisons.foreach(c =>
      if (c > 0) {
        return false
      } else if (c < 0) {
        return true
    })
    false
  }

  def gt(a: List[Double], b: List[Double]): Boolean = {
    val maxSize = Math.max(a.length, b.length)
    val aFull = a.padTo(maxSize, 0.0)
    val bFull = b.padTo(maxSize, 0.0)
    val comparisons = aFull.zip(bFull).map(ab => ab._1 - ab._2)
    comparisons.foreach(c =>
      if (c < 0) {
        return false
      } else if (c > 0) {
        return true
    })
    false
  }

  def sortDistances(distances: List[(List[Double], Int)]): Array[(List[Double], Int)] =
    Sorting.stableSort(distances, (a, b) => lt(a._1, b._1))

}
