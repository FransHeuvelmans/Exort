package dev.hillman.exort

import dev.hillman.exort.Tools.sortKeyType
import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.nio.file.Paths

class ExtSortStringTest extends AnyFlatSpec with BeforeAndAfter {

  def testFile(fileToCheck: File, answers: List[String], keyLoc: Int): Unit = {
    val parser = Tools.tsvParser(false)
    parser.beginParsing(fileToCheck)
    var ansNr = 0
    var currentRow: Array[String] = parser.parseNext()
    while (currentRow != null) {
      assert(currentRow(keyLoc) === answers(ansNr))
      ansNr += 1
      currentRow = parser.parseNext()
    }
    parser.stopParsing()
  }

  val filesToRemove = List(
    "p0.tsv",
    "p1.tsv",
    "p2.tsv",
    "p1000.tsv",
    "p1001.tsv",
    "p2000.tsv",
    "testfileB_sorted.tsv"
  )

  after {
    filesToRemove
      .map(fileName => new File(testDirectory.toString + "/" + fileName))
      .foreach(f => if (f.exists()) { f.delete() })
  }

  val testDirectory = Paths.get("src/test/resources").toAbsolutePath
  val testFileA = new File(testDirectory.toString + "/testfileB.csv")

  "Sorting using a case sensitive key type" should "result in a correct answer" in {
    val settingsNormal = ExortSetting(
      List(testFileA),
      rowSplit = 3,
      keyType = Array(sortKeyType.stringKeyType),
      sep = ',',
      keyNr = Array(1),
      outFileName = "testfileB_sorted.tsv"
    )
    val outFiles = ExtSort.sortParts(testDirectory, settingsNormal)

    val p0Vals = "Db" :: "Za" :: "ba" :: Nil
    val p1Vals = "Ab" :: "CC" :: "aa" :: Nil
    val p2Vals = "BB" :: "DA" :: "cc" :: Nil

    testFile(outFiles(0).file, p0Vals, settingsNormal.keyNr.head)
    testFile(outFiles(1).file, p1Vals, settingsNormal.keyNr.head)
    testFile(outFiles(2).file, p2Vals, settingsNormal.keyNr.head)

    val mergeList = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
    assert(mergeList.length === outFiles.length) // Nothing to be merged here

    val f = ExtSort.externalMergeSort(mergeList, testDirectory, settingsNormal)

    val pFinalVals = "Ab" :: "BB" :: "CC" :: "DA" :: "Db" :: "Za" :: "aa" :: "ba" :: "cc" :: Nil
    testFile(f, pFinalVals, settingsNormal.keyNr.head)
  }

  "Sorting using a case insensitive key type" should "result in a correct answer" in {
    val settingsNormal = ExortSetting(
      List(testFileA),
      rowSplit = 3,
      keyType = Array(sortKeyType.stringLowerCaseKeyType),
      sep = ',',
      keyNr = Array(1),
      outFileName = "testfileB_sorted.tsv"
    )
    val outFiles = ExtSort.sortParts(testDirectory, settingsNormal)

    val p0Vals = "ba" :: "Db" :: "Za" :: Nil
    val p1Vals = "aa" :: "Ab" :: "CC" :: Nil
    val p2Vals = "BB" :: "cc" :: "DA" :: Nil

    testFile(outFiles(0).file, p0Vals, settingsNormal.keyNr.head)
    testFile(outFiles(1).file, p1Vals, settingsNormal.keyNr.head)
    testFile(outFiles(2).file, p2Vals, settingsNormal.keyNr.head)

    val mergeList = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
    assert(mergeList.length === outFiles.length) // Nothing to be merged here

    val f = ExtSort.externalMergeSort(mergeList, testDirectory, settingsNormal)

    val pFinalVals = "aa" :: "Ab" :: "ba" :: "BB" :: "CC" :: "cc" :: "DA" :: "Db" :: "Za" :: Nil
    testFile(f, pFinalVals, settingsNormal.keyNr.head)
  }

}
