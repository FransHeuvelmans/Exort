package dev.hillman.exort

import java.io.File
import java.nio.file.Paths

import dev.hillman.exort.Tools.sortKeyType
import org.scalatest.flatspec.AnyFlatSpec

class ExtSortBadTest extends AnyFlatSpec {
  "A file with weird lines" should "be parsable" in {
    val testDirectory = Paths.get("src/test/resources").toAbsolutePath
    val testFile = new File(testDirectory.toString + "/testBadFile.csv")
    val settings = ExortSetting(List(testFile),
                                keyType = Array(sortKeyType.integerKeyType),
                                keyNr = Array(4),
                                outFileName = "testfileBad_sorted.tsv")
    val outFiles = ExtSort.sortParts(testDirectory, settings)
    outFiles.foreach(_.file.delete())
  }

  it should "also be parseable as VaryRows" in {
    val testDirectory = Paths.get("src/test/resources").toAbsolutePath
    val testFile = new File(testDirectory.toString + "/testBadFile.csv")
    val settings = ExortSetting(
      List(testFile),
      keyType = Array(sortKeyType.integerKeyType, sortKeyType.stringKeyType),
      keyNr = Array(4, 3),
      outFileName = "testfileBad_sorted.tsv")
    val outFiles = ExtSort.sortParts(testDirectory, settings)
    outFiles.foreach(_.file.delete())
  }

}
