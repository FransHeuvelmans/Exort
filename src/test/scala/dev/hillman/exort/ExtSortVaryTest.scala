package dev.hillman.exort

import java.io.File
import java.nio.file.Paths

import dev.hillman.exort.Tools.sortKeyType
import org.scalatest.flatspec.AnyFlatSpec

class ExtSortVaryTest extends AnyFlatSpec {

  /**
    * Test a list of answers if they are correct
    * @param fileToCheck File that has been sorted
    * @param answers the answers as they should be in the file
    * @param keyLoc location of the 2 answers in the tsv file
    */
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
  val testFileB2 = new File(testDirectory.toString + "/testFileB2.csv")

  val allAtOnceSetting = ExortSetting(
    List(testFileVary),
    keyType = Array(sortKeyType.stringKeyType, sortKeyType.integerNegKeyType),
    keyNr = Array(1, 0),
    outFileName = "testfileVary_sorted.csv"
  )
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

  val splitInTwoSetting = allAtOnceSetting.copy(rowSplit = 5)
  val firstResult = List(
    (8, "a"),
    (6, "b"),
    (4, "b"),
    (12, "c"),
    (3, "c")
  )
  val secondResult = List(
    (10, "a"),
    (9, "a"),
    (2, "a"),
    (11, "b"),
    (-5, "b")
  )

  it should "be transformed into 2 sorted parts" in {
    val outFiles = ExtSort.sortParts(testDirectory, splitInTwoSetting)
    assert(outFiles.length === 2)
    val firstFile = outFiles.head.asInstanceOf[VarySortedFile]
    // Note that varyFiles only keep track of start and end values
    assert(firstFile.vstart.v3.head === 8)
    assert(firstFile.vstart.v1.head === "a")
    assert(firstFile.vstart.v2 === Nil)
    assert(firstFile.vend.v3.head === 3)
    assert(firstFile.vend.v1.head === "c")
    testFile(firstFile.file, firstResult, (0, 1))

    val lastFile = outFiles.tail.head.asInstanceOf[VarySortedFile]
    testFile(lastFile.file, secondResult, (0, 1))
    outFiles.foreach(_.file.delete())
  }

  "the two sorted parts" should "not be mergable without sorting" in {
    val outFiles = ExtSort.sortParts(testDirectory, splitInTwoSetting)
    val mergeList = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
    assert(mergeList.length === 2)
    outFiles.foreach(_.file.delete())
  }

  it should "be mergable with sorting" in {
    val outFiles = ExtSort.sortParts(testDirectory, splitInTwoSetting)
    val processedFile =
      ExtSort.externalMergeSort(outFiles.map(_.file), testDirectory, splitInTwoSetting)
    testFile(processedFile, fullResult, (0, 1))
    processedFile.delete()
    outFiles.foreach(_.file.delete())
  }

  "A file with a string plus int combination" should "sort case sensitive properly" in {
    val settingsNormal = ExortSetting(
      List(testFileB2),
      rowSplit = 4,
      keyType = Array(sortKeyType.stringNegKeyType, sortKeyType.integerKeyType),
      sep = ' ',
      keyNr = Array(1, 0),
      outFileName = "testfileB2_sorted.tsv"
    )
    val outFiles = ExtSort.sortParts(testDirectory, settingsNormal)

    val p0Vals = List(
      (200, "z"),
      (504, "g"),
      (896, "a"),
      (100, "Hz")
    )
    val p1Vals = List(
      (100, "ha"),
      (102, "g"),
      (101, "Z"),
      (286, "A")
    )

    testFile(outFiles(0).file, p0Vals, (0, 1))
    testFile(outFiles(1).file, p1Vals, (0, 1))

    val mergeList = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
    assert(mergeList.length === outFiles.length) // Nothing to be merged here

    val f = ExtSort.externalMergeSort(mergeList, testDirectory, settingsNormal)

    val pFinalVals = List(
      (200, "z"),
      (100, "ha"),
      (102, "g"),
      (504, "g"),
      (896, "a"),
      (101, "Z"),
      (100, "Hz"),
      (286, "A")
    )
    testFile(f, pFinalVals, (0, 1))
    mergeList.foreach(_.delete())
    f.delete()
  }

  it should "sort case insensitive properly" in {
    val settingsNormal = ExortSetting(
      List(testFileB2),
      rowSplit = 4,
      keyType = Array(sortKeyType.stringLowerCaseNegKeyType, sortKeyType.integerKeyType),
      sep = ' ',
      keyNr = Array(1, 0),
      outFileName = "testfileB2_sorted.tsv"
    )
    val outFiles = ExtSort.sortParts(testDirectory, settingsNormal)

    val p0Vals = List(
      (200, "z"),
      (100, "Hz"),
      (504, "g"),
      (896, "a")
    )
    val p1Vals = List(
      (101, "Z"),
      (100, "ha"),
      (102, "g"),
      (286, "A")
    )

    testFile(outFiles(0).file, p0Vals, (0, 1))
    testFile(outFiles(1).file, p1Vals, (0, 1))

    val mergeList = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
    assert(mergeList.length === outFiles.length) // Nothing to be merged here

    val f = ExtSort.externalMergeSort(mergeList, testDirectory, settingsNormal)

    val pFinalVals = List(
      (101, "Z"),
      (200, "z"),
      (100, "Hz"),
      (100, "ha"),
      (102, "g"),
      (504, "g"),
      (286, "A"),
      (896, "a")
    )
    testFile(f, pFinalVals, (0, 1))
    mergeList.foreach(_.delete())
    f.delete()
  }

}
