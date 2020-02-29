package dev.hillman.exort

import java.io.File
import java.nio.file.Paths

import dev.hillman.exort.Tools.sortKeyType
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class ExtSortVaryTest extends FlatSpec with Matchers {

  def testFile(fileToCheck: File, answers: List[(Int, String)], keyLoc: (Int, Int)): Unit = {
    val parser = Tools.tsvParser(false)
    parser.beginParsing(fileToCheck)
    var ansNr = 0
    var currentRow: Array[String] = parser.parseNext()
    while (currentRow != null) {
      assert(currentRow(keyLoc._1).toInt === answers(ansNr)._1)
      assert(currentRow(keyLoc._2) === answers(ansNr)._2)
      ansNr += 1
      currentRow = parser.parseNext()
    }
    parser.stopParsing()
  }

  val testDirectory = Paths.get("src/test/resources").toAbsolutePath
  val testFileVary = new File(testDirectory.toString + "/testfileVary.csv")

  val allAtOnceSetting = ExortSetting(testFileVary,
    keyType = sortKeyType.stringKeyType :: sortKeyType.integerNegKeyType :: Nil,
    keyNr = 1 :: 0 :: Nil)

  val fullResult = List(
    (10, "a"),
    (9, "a"),
    (8, "a"),
    (2, "a"),
    (11, "b"),
    (6, "b"),
    (4, "b"),
    (-5, "b"),
    (12, "c"),
    (3, "c")
  )

  "a non-sorted csv with vary setting" should "be sortable in one go" in {
    val outFiles = ExtSort.sortParts(testDirectory, allAtOnceSetting)
    assert(outFiles.length === 1)
    testFile(outFiles.head.file, fullResult, (0, 1))
    outFiles.head.file.delete()
  }
}
