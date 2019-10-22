package dev.hillman.exort
import dev.hillman.exort.Tools.sortKeyType
import java.io.File
import java.nio.file.{Path, Paths}

import org.scalatest.{FlatSpec, FunSpec, Matchers}

class ExtSortTest extends FlatSpec with Matchers {

  def testFile(fileToCheck: File, answers: List[Int], keyLoc: Int): Unit = {
    val parser = Tools.tsvParser(false)
    parser.beginParsing(fileToCheck)
    var ansNr = 0
    var currentRow: Array[String] = parser.parseNext()
    while (currentRow != null) {
      currentRow(keyLoc) === answers(ansNr)
      ansNr += 1
      currentRow = parser.parseNext()
    }
    parser.stopParsing()
  }

    val testDirectory = Paths.get("src/test/resources").toAbsolutePath
    val settings = ExortSetting(new File(testDirectory.toString + "/testfileA.csv"),
      6,
      keyType = sortKeyType.integerKeyType :: Nil,
      keyNr = 1 :: Nil)
    val outFiles = ExtSort.sortParts(testDirectory, settings)
    val p0Vals = 7 :: 9 :: 10 :: 11 :: 12 :: 13 :: Nil
    val p1Vals = 78 :: 97 :: 100 :: 101 :: 105 :: 110 :: Nil

    "A non-sorted input csv" should "be sortable in parts" in {
      testFile(outFiles(0).file, p0Vals, settings.keyNr(0))
      testFile(outFiles(1).file, p1Vals, settings.keyNr(0))
    }

    it should "Should be mergeable if the parts do not overlap" in {
      val o = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
      testFile(o(0), p0Vals ++ p1Vals, settings.keyNr(0))
      o(0).delete()
    }

    it should "Be mergeable by joining the parts together" in {
      val f = ExtSort.externalMergeSort(outFiles.map(_.file), testDirectory, settings)
      testFile(f, p0Vals ++ p1Vals, settings.keyNr(0))
      f.delete()
      outFiles.map(_.file.delete())
    }
}
