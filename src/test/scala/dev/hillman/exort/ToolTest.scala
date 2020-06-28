package dev.hillman.exort

import dev.hillman.exort.Tools.sortKeyType
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ToolTest extends AnyFlatSpec with Matchers {

  "String-Array with settings " should "be convertable to VaryRows" in {
    val sortTypeList = Array(sortKeyType.integerKeyType,
                             sortKeyType.stringKeyType,
                             sortKeyType.integerNegKeyType)
    val settings = ExortSetting(null,
                                rowSplit = 1000000,
                                keyType = sortTypeList,
                                keyNr = Array(3, 1, 0),
                                outFileName = "")
    val data = Array("120", "abc", "12.34", "56")
    val row = Tools.convertToVaryRow(data, settings).toOption.get
    assert(row.v1(0) === "abc")
    assert(row.v3 === List(56, 120))
    assert(row.vs === sortTypeList)
  }

  it should "be convertable in ComplexVaryRows" in {
    val data = Array("120", "abc", "12.34", "56")
    val sortTypeList = Array(sortKeyType.integerKeyType,
                             sortKeyType.stringKeyType,
                             sortKeyType.integerNegKeyType,
                             sortKeyType.decimalKeyType)
    val sortLocationList = Array(3, 1, 0, 2)
    val settings = ExortSetting(null,
                                rowSplit = 1000000,
                                keyType = sortTypeList,
                                keyNr = sortLocationList,
                                outFileName = "")
    val row = Tools.convertToComplexVaryRow(data, settings).toOption.get
    assert(row.v1(0) === "abc")
    assert(row.v3 === List(BigInt(56), BigInt(120)))
    assert(row.v2 === List(BigDecimal(12.34)))
    assert(row.vs === sortTypeList)
  }

}
