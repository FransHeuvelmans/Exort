package dev.hillman.exort

object ComplexVROrdering extends Ordering[VaryRowComplex] {
  @scala.annotation.tailrec
  override def compare(x: VaryRowComplex, y: VaryRowComplex): Int = x.vs.head match {
    case Tools.sortKeyType.stringKeyType => {
      val cmp = Ordering.String.compare(x.v1.head, y.v1.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRowComplex(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
      val newThat = VaryRowComplex(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
      ComplexVROrdering.compare(newThis, newThat)
    }
    case Tools.sortKeyType.stringNegKeyType => {
      val cmp = -Ordering.String.compare(x.v1.head, y.v1.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRowComplex(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
      val newThat = VaryRowComplex(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
      ComplexVROrdering.compare(newThis, newThat)
    }
    case Tools.sortKeyType.decimalKeyType => {
      val cmp = Ordering.BigDecimal.compare(x.v2.head, y.v2.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRowComplex(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
      val newThat = VaryRowComplex(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
      ComplexVROrdering.compare(newThis, newThat)
    }
    case Tools.sortKeyType.decimalNegKeyType => {
      val cmp = -Ordering.BigDecimal.compare(x.v2.head, y.v2.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRowComplex(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
      val newThat = VaryRowComplex(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
      ComplexVROrdering.compare(newThis, newThat)
    }
    case Tools.sortKeyType.integerKeyType => {
      val cmp = Ordering.BigInt.compare(x.v3.head, y.v3.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRowComplex(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
      val newThat = VaryRowComplex(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
      ComplexVROrdering.compare(newThis, newThat)
    }
    case Tools.sortKeyType.integerNegKeyType => {
      val cmp = -Ordering.BigInt.compare(x.v3.head, y.v3.head)
      if (cmp != 0) {
        return cmp
      }
      val newThis = VaryRowComplex(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
      val newThat = VaryRowComplex(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
      ComplexVROrdering.compare(newThis, newThat)
    }
  }

  @scala.annotation.tailrec
  def distance(x: VaryRowComplex,
               y: VaryRowComplex,
               prevDistance: List[BigDecimal]): List[BigDecimal] = {
    if (x.vs.isEmpty) {
      return prevDistance.reverse
    }
    x.vs.head match {
      case Tools.sortKeyType.stringKeyType => {
        val cmp = Tools.StringDistanceLowerCase(x.v1.head, y.v1.head)
        val newThis = VaryRowComplex(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
        val newThat = VaryRowComplex(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
        ComplexVROrdering.distance(newThis,
          newThat,
          cmp.map(BigDecimal(_)) ::: prevDistance)
      }
      case Tools.sortKeyType.stringNegKeyType => {
        val cmp = Tools.StringDistanceLowerCase(x.v1.head, y.v1.head).map(-_)
        val newThis = VaryRowComplex(x.v1.tail, x.v2, x.v3, x.vs.tail, x.content)
        val newThat = VaryRowComplex(y.v1.tail, y.v2, y.v3, y.vs.tail, y.content)
        ComplexVROrdering.distance(newThis,
          newThat,
          cmp.map(BigDecimal(_)) ::: prevDistance)
      }
      case Tools.sortKeyType.decimalKeyType => {
        val cmp = x.v2.head - y.v2.head
        val newThis = VaryRowComplex(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
        val newThat = VaryRowComplex(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
        ComplexVROrdering.distance(newThis, newThat, cmp :: prevDistance)
      }
      case Tools.sortKeyType.decimalNegKeyType => {
        val cmp = -(x.v2.head - y.v2.head)
        val newThis = VaryRowComplex(x.v1, x.v2.tail, x.v3, x.vs.tail, x.content)
        val newThat = VaryRowComplex(y.v1, y.v2.tail, y.v3, y.vs.tail, y.content)
        ComplexVROrdering.distance(newThis, newThat, cmp :: prevDistance)
      }
      case Tools.sortKeyType.integerKeyType => {
        val cmp = x.v3.head - y.v3.head
        val newThis = VaryRowComplex(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
        val newThat = VaryRowComplex(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
        ComplexVROrdering.distance(newThis, newThat, BigDecimal(cmp) :: prevDistance)
      }
      case Tools.sortKeyType.integerNegKeyType => {
        val cmp = -(x.v3.head - y.v3.head)
        val newThis = VaryRowComplex(x.v1, x.v2, x.v3.tail, x.vs.tail, x.content)
        val newThat = VaryRowComplex(y.v1, y.v2, y.v3.tail, y.vs.tail, y.content)
        ComplexVROrdering.distance(newThis, newThat, BigDecimal(cmp) :: prevDistance)
      }
      case _ => prevDistance.reverse
    }
  }
}