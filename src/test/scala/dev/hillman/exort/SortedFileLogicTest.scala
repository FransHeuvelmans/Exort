package dev.hillman.exort

import org.scalatest.FlatSpec

class SortedFileLogicTest extends FlatSpec {

  "a increasing list of separated LongFiles" should "be completely mergable" in {
    val testFiles: List[TempSortedFile] = List(LongSortedFile(0, 5, null),
                                               LongSortedFile(6, 10, null),
                                               LongSortedFile(11, 15, null))
    val compareFile: TempSortedFile = LongSortedFile(16, 100, null)
    val results = testFiles.map(compareFile.distance(_))
    results.foreach(r => assert(r.head > 0))
    results.sliding(2).foreach(r2 => r2(0).head > r2(1).head)
  }

  "a list of overlapping Longfiles" should "be equal in all cases in either direction" in {
    val testFiles: List[TempSortedFile] =
      List(LongSortedFile(0, 8, null), LongSortedFile(14, 25, null))
    val compareFile: TempSortedFile = LongSortedFile(5, 15, null)
    val results = testFiles.map(compareFile.distance(_))
    results.foreach(r => assert(r.head == 0))
    val reverseResults = testFiles.map(compareFile.distance(_, true))
    reverseResults.foreach(r => assert(r.head === 0))
  }

  "a decreasing list of separated Longfiles" should "be completely mergable" in {
    val testFiles: List[TempSortedFile] = List(LongSortedFile(16, 20, null),
                                               LongSortedFile(11, 15, null),
                                               LongSortedFile(6, 10, null))
    val compareFile: TempSortedFile = LongSortedFile(-100, 5, null)
    val results = testFiles.map(compareFile.distance(_, true))
    results.foreach(r => assert(r.head > 0))
    results.sliding(2).foreach(r2 => r2(0).head > r2(1).head)
  }

  "lists of separated VaryFiles" should "be completely mergable" in {
    val sortRules = Tools.sortKeyType.stringKeyType :: Tools.sortKeyType.integerKeyType :: Nil
    val testOriginalFiles = List(
      VarySortedFile(VaryRow("a" :: Nil, Nil, 2 :: Nil, sortRules, null),
                     VaryRow("a" :: Nil, Nil, 5 :: Nil, sortRules, null),
                     null),
      VarySortedFile(VaryRow("a" :: Nil, Nil, 6 :: Nil, sortRules, null),
                     VaryRow("a" :: Nil, Nil, 20 :: Nil, sortRules, null),
                     null),
      VarySortedFile(VaryRow("b" :: Nil, Nil, -8 :: Nil, sortRules, null),
                     VaryRow("c" :: Nil, Nil, 15 :: Nil, sortRules, null),
                     null),
      VarySortedFile(VaryRow("d" :: Nil, Nil, -8 :: Nil, sortRules, null),
                     VaryRow("e" :: Nil, Nil, 15 :: Nil, sortRules, null),
                     null),
      VarySortedFile(VaryRow("e" :: Nil, Nil, 20 :: Nil, sortRules, null),
                     VaryRow("f" :: Nil, Nil, -100 :: Nil, sortRules, null),
                     null)
    )
    val testFiles: List[TempSortedFile] = testOriginalFiles
    val compareFile =
      VarySortedFile(VaryRow("a" :: Nil, Nil, -2000 :: Nil, sortRules, null),
                     VaryRow("a" :: Nil, Nil, -100 :: Nil, sortRules, null),
                     null)
    val normalResults = testFiles.map(compareFile.distance(_))
    normalResults.foreach(r => assert(FileSort.lt(r, Nil)))
    normalResults.sliding(2).foreach(r2 => assert(FileSort.gt(r2(0), r2(1))))
    val reverseSortRules = Tools.sortKeyType.stringNegKeyType :: Tools.sortKeyType.integerNegKeyType :: Nil

    val reverseCompareFile =
      VarySortedFile(compareFile.vlow.copy(vs = reverseSortRules),
                     compareFile.vhigh.copy(vs = reverseSortRules),
                     compareFile.file)
    val reverseFiles = testOriginalFiles.map(
      tf =>
        VarySortedFile(tf.vlow.copy(vs = reverseSortRules),
                       tf.vhigh.copy(vs = reverseSortRules),
                       tf.file))
    val reverseResults = reverseFiles.map(reverseCompareFile.distance(_))
    reverseResults.foreach(r => assert(FileSort.gt(r, Nil)))
    reverseResults.sliding(2).foreach(r2 => assert(FileSort.lt(r2(0), r2(1))))
  }
}
