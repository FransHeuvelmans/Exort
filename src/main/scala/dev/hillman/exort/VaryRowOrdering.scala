package dev.hillman.exort

object VaryRowOrdering extends Ordering[VaryRow] {
  @scala.annotation.tailrec
  override def compare(x: VaryRow, y: VaryRow): Int =
    x.vs.head match {
      case Tools.sortKeyType.stringKeyType | Tools.sortKeyType.stringLowerCaseKeyType => {
        // toLowerCase is already applied at loading stage
        val cmp = Ordering.String.compare(x.v1.head, y.v1.head)
        if (cmp != 0) {
          return cmp
        }
        val newThis = VaryRow(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.compare(newThis, newThat)
      }
      case Tools.sortKeyType.stringNegKeyType | Tools.sortKeyType.stringLowerCaseNegKeyType => {
        // toLowerCase is already applied at loading stage
        val cmp = -Ordering.String.compare(x.v1.head, y.v1.head)
        if (cmp != 0) {
          return cmp
        }
        val newThis = VaryRow(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.compare(newThis, newThat)
      }
      case Tools.sortKeyType.decimalKeyType => {
        val cmp = Ordering.Double.TotalOrdering.compare(x.v2.head, y.v2.head)
        if (cmp != 0) {
          return cmp
        }
        val newThis = VaryRow(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.compare(newThis, newThat)
      }
      case Tools.sortKeyType.decimalNegKeyType => {
        val cmp = -Ordering.Double.TotalOrdering.compare(x.v2.head, y.v2.head)
        if (cmp != 0) {
          return cmp
        }
        val newThis = VaryRow(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.compare(newThis, newThat)
      }
      case Tools.sortKeyType.integerKeyType => {
        val cmp = Ordering.Long.compare(x.v3.head, y.v3.head)
        if (cmp != 0) {
          return cmp
        }
        val newThis = VaryRow(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
        VaryRowOrdering.compare(newThis, newThat)
      }
      case Tools.sortKeyType.integerNegKeyType => {
        val cmp = -Ordering.Long.compare(x.v3.head, y.v3.head)
        if (cmp != 0) {
          return cmp
        }
        val newThis = VaryRow(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
        VaryRowOrdering.compare(newThis, newThat)
      }
    }

  @scala.annotation.tailrec
  def distance(x: VaryRow, y: VaryRow, prevDistance: List[Double]): List[Double] = {
    if (x.vs.isEmpty) {
      return prevDistance.reverse
    }
    x.vs.head match {
      case Tools.sortKeyType.stringKeyType | Tools.sortKeyType.stringLowerCaseKeyType => {
        // toLowerCase is already applied at loading stage
        val cmp = Tools.StringDistance(x.v1.head, y.v1.head)
        val newThis = VaryRow(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.distance(newThis, newThat, cmp.map(_.toDouble) ::: prevDistance)
      }
      case Tools.sortKeyType.stringNegKeyType | Tools.sortKeyType.stringLowerCaseNegKeyType => {
        // toLowerCase is already applied at loading stage
        val cmp = Tools.StringDistance(x.v1.head, y.v1.head).map(-_)
        val newThis = VaryRow(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.distance(newThis, newThat, cmp.map(_.toDouble) ::: prevDistance)
      }
      case Tools.sortKeyType.decimalKeyType => {
        val cmp = x.v2.head - y.v2.head
        val newThis = VaryRow(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.distance(newThis, newThat, cmp :: prevDistance)
      }
      case Tools.sortKeyType.decimalNegKeyType => {
        val cmp = -(x.v2.head - y.v2.head)
        val newThis = VaryRow(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
        VaryRowOrdering.distance(newThis, newThat, cmp :: prevDistance)
      }
      case Tools.sortKeyType.integerKeyType => {
        val cmp = x.v3.head - y.v3.head
        val newThis = VaryRow(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
        VaryRowOrdering.distance(newThis, newThat, cmp.toDouble :: prevDistance)
      }
      case Tools.sortKeyType.integerNegKeyType => {
        val cmp = -(x.v3.head - y.v3.head)
        val newThis = VaryRow(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
        val newThat = VaryRow(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
        VaryRowOrdering.distance(newThis, newThat, cmp.toDouble :: prevDistance)
      }
      case _ => prevDistance.reverse
    }
  }
}
