package dev.hillman.exort
import dev.hillman.exort.Tools.sortKeyType
import java.io.File
import java.nio.file.Paths

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter

class ExtSortIntTest extends AnyFlatSpec with BeforeAndAfter {

  def testFile(fileToCheck: File, answers: List[Int], keyLoc: Int): Unit = {
    val parser = Tools.tsvParser(false)
    parser.beginParsing(fileToCheck)
    var ansNr = 0
    var currentRow: Array[String] = parser.parseNext()
    while (currentRow != null) {
      assert(currentRow(keyLoc) === answers(ansNr).toString)
      ansNr += 1
      currentRow = parser.parseNext()
    }
    parser.stopParsing()
  }

  val testDirectory = Paths.get("src/test/resources").toAbsolutePath
  val testFileA = new File(testDirectory.toString + "/testfileA.csv")
  val settings = ExortSetting(testFileA,
                              rowSplit = 6,
                              keyType = Array(sortKeyType.integerKeyType),
                              keyNr = Array(1),
                              outFileName = "testfileA_sorted.tsv")
  var outFiles: List[TempSortedFile] = _
  val p0Vals = 7 :: 9 :: 10 :: 11 :: 12 :: 13 :: Nil
  val p1Vals = 78 :: 97 :: 100 :: 101 :: 105 :: 110 :: Nil

  before {
    outFiles = ExtSort.sortParts(testDirectory, settings)
  }
  after {
    outFiles.foreach(_.file.delete())
  }

  "A non-sorted input csv" should "be sortable in parts" in {
    testFile(outFiles(0).file, p0Vals, settings.keyNr.head)
    testFile(outFiles(1).file, p1Vals, settings.keyNr.head)
    val longFiles = outFiles.map(_.asInstanceOf[LongSortedFile])
    assert(longFiles.head.vlow === 7)
    assert(longFiles.head.vhigh === 13)
    assert(longFiles.tail.head.vlow === 78)
    assert(longFiles.tail.head.vhigh === 110)
  }

  it should "Should be mergeable if the parts do not overlap" in {
    val o = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
    testFile(o.head, p0Vals ++ p1Vals, settings.keyNr.head)
    o.head.delete()
  }

  it should "Be mergeable by joining the parts together" in {
    val f =
      ExtSort.externalMergeSort(outFiles.map(_.file), testDirectory, settings)
    testFile(f, p0Vals ++ p1Vals, settings.keyNr.head)
    f.delete()
  }

  val reverseSettings = ExortSetting(testFileA,
                                     rowSplit = 6,
                                     keyType =
                                       Array(sortKeyType.integerNegKeyType),
                                     keyNr = Array(1),
                                     outFileName = "testfileA_sorted.tsv")

  "A non-sorted inputfile" should "be reverse-sortable" in {
    val outFilesReverse = ExtSort
      .sortParts(testDirectory, reverseSettings)
      .map(_.asInstanceOf[LongSortedFile])
    testFile(outFilesReverse(0).file, p0Vals.reverse, settings.keyNr.head)
    testFile(outFilesReverse(1).file, p1Vals.reverse, settings.keyNr.head)
    assert(outFilesReverse.head.vlow === 7)
    assert(outFilesReverse.head.vhigh === 13)
    assert(outFilesReverse.tail.head.vlow === 78)
    assert(outFilesReverse.tail.head.vhigh === 110)
    outFilesReverse.foreach(_.file.delete())
  }

}
