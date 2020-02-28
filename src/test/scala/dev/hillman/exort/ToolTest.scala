package dev.hillman.exort

import dev.hillman.exort.Tools.sortKeyType
import org.scalatest.{FlatSpec, Matchers}

class ToolTest extends FlatSpec with Matchers {

  "String-Array with settings " should "be convertable to VaryRows" in {
    val sortTypeList = sortKeyType.integerKeyType :: sortKeyType.stringKeyType :: sortKeyType.integerNegKeyType :: Nil
    val settings = ExortSetting(null,
      rowSplit = 1000000,
      keyType = sortTypeList,
      keyNr = 3 :: 1 :: 0 :: Nil)
    val data = Array("120", "abc", "12.34", "56")
    val row = Tools.convertToVaryRow(data, settings)
    assert(row.v1(0) === "abc")
    assert(row.v3 === List(56, 120))
    assert(row.vs === sortTypeList)
  }

}
