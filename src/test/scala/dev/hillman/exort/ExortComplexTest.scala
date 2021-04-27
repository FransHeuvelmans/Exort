package dev.hillman.exort

import org.scalatest.BeforeAndAfter
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.nio.file.Paths

class ExortComplexTest extends AnyFlatSpec with BeforeAndAfter {

  /**
    * Test a list of answers if they are correct
    * @param fileToCheck File that has been sorted
    * @param answers the answers as they should be in the file
    * @param keyLoc location of the 2 answers in the tsv file
    */
  def testFile(fileToCheck: File, answers: List[(BigInt, String)], keyLoc: (Int, Int)): Unit = {
    val parser = Tools.tsvParser(false)
    parser.beginParsing(fileToCheck)
    var ansNr = 0
    var currentRow: Array[String] = parser.parseNext()
    while (currentRow != null) {
      assert(BigInt(currentRow(keyLoc._1)) === answers(ansNr)._1)
      assert(currentRow(keyLoc._2) === answers(ansNr)._2)
      ansNr += 1
      currentRow = parser.parseNext()
    }
    parser.stopParsing()
  }

  val outFileName = "testfileC_sorted.tsv"
  val filesToRemove = List(
    "p0.tsv",
    "p1.tsv",
    "p1000.tsv",
    outFileName
  )

  after {
    filesToRemove
      .map(fileName => new File(testDirectory.toString + "/" + fileName))
      .foreach(f => if (f.exists()) { f.delete() })
  }

  val testDirectory = Paths.get("src/test/resources").toAbsolutePath
  val testFileComplex = new File(testDirectory.toString + "/testFileC.csv")

  "Sorting with very large numbers" should "go successful with the right result" in {
    val settingsNormal = ExortSetting(
      List(testFileComplex),
      rowSplit = 4,
      keyType = Array(Tools.sortKeyType.integerKeyType, Tools.sortKeyType.stringNegKeyType),
      sep = '/',
      keyNr = Array(1, 0),
      outFileName = outFileName,
      complexSort = true
    )
    val outFiles = ExtSort.sortParts(testDirectory, settingsNormal)

    val p0Vals = List(
      (BigInt("4611686014729567292565492645190374610238454"), "c"),
      (BigInt("4611686014729567292565492645190374610238455"), "b"),
      (BigInt("4611686014729567292565492645190374610238456"), "a"),
      (BigInt("4611686014729567292565492645190374610238481"), "d")
    )
    val p1Vals = List(
      (BigInt("4511686014729567292565492645190374610238499"), "g"),
      (BigInt("4611686014729567292565492645190374610238401"), "f"),
      (BigInt("4611686014729567292565492645190374610238456"), "e"),
      (
        BigInt(
          "8912730218936492183745612038496239874632987463028471293786412987346120347120398498324752034857039485700284765234560"
        ),
        "h"
      )
    )

    testFile(outFiles(0).file, p0Vals, (1, 0))
    testFile(outFiles(1).file, p1Vals, (1, 0))

    val mergeList = ExtSort.mergeAccidentalSorted(outFiles, testDirectory)
    assert(mergeList.length === outFiles.length) // Nothing to be merged here

    val f = ExtSort.externalMergeSort(mergeList, testDirectory, settingsNormal)

    val pFinalVals = List(
      (BigInt("4511686014729567292565492645190374610238499"), "g"),
      (BigInt("4611686014729567292565492645190374610238401"), "f"),
      (BigInt("4611686014729567292565492645190374610238454"), "c"),
      (BigInt("4611686014729567292565492645190374610238455"), "b"),
      (BigInt("4611686014729567292565492645190374610238456"), "e"),
      (BigInt("4611686014729567292565492645190374610238456"), "a"),
      (BigInt("4611686014729567292565492645190374610238481"), "d"),
      (
        BigInt(
          "8912730218936492183745612038496239874632987463028471293786412987346120347120398498324752034857039485700284765234560"
        ),
        "h"
      )
    )
    testFile(f, pFinalVals, (1, 0))
  }

}
