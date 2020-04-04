package dev.hillman.exort

import java.io.File
import java.nio.file.Paths

import dev.hillman.exort.Tools.sortKeyType
import org.scalatest.FlatSpec

class ExtSortBadTest extends FlatSpec {
  "A file with weird lines" should "be parsable" in {
    val testDirectory = Paths.get("src/test/resources").toAbsolutePath
    val testFile = new File(testDirectory.toString + "/testBadFile.csv")
    val settings = ExortSetting(testFile,
                                keyType = sortKeyType.integerKeyType :: Nil,
                                keyNr = 4 :: Nil,
                                outFileName = "testfileBad_sorted.tsv")
    val outFiles = ExtSort.sortParts(testDirectory, settings)
    outFiles.foreach(_.file.delete())
  }

  it should "also be parseable as VaryRows" in {
    val testDirectory = Paths.get("src/test/resources").toAbsolutePath
    val testFile = new File(testDirectory.toString + "/testBadFile.csv")
    val settings = ExortSetting(
      testFile,
      keyType = sortKeyType.integerKeyType :: sortKeyType.stringKeyType :: Nil,
      keyNr = 4 :: 3 :: Nil,
      outFileName = "testfileBad_sorted.tsv")
    val outFiles = ExtSort.sortParts(testDirectory, settings)
    outFiles.foreach(_.file.delete())
  }

}
