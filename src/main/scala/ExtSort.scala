import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.nio.file.{Files, Path, Paths}

import com.univocity.parsers.common.AbstractParser
import com.univocity.parsers.tsv.{TsvParser, TsvWriter, TsvWriterSettings}

import scala.collection.{LinearSeq, mutable}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

case class TempSortedFile(file: File, low: Long, high: Long)

object ExtSort {

  def sortParts(rootLocation: Path, settings: ExortSetting): List[TempSortedFile] = {

    val parser = settings.sep match {
      case '\t' => Tools.tsvParser(settings.skipHeader)
      case _ => Tools.csvParser(settings.sep, settings.skipHeader)
    }

    parser.beginParsing(settings.file)
    var iterRow = 0
    var iterFile = 0
    var sortedFiles: List[TempSortedFile] = Nil
    var currentRow: Array[String] = parser.parseNext()
    // TODO: Really need to learn how to work around type erasure (!!!)
    settings.keyType match {
      case Tools.sortKeyType.numericKeyType => {
        var currentDataSet: List[LongRow] = Nil
        while (currentRow != null) {
          currentDataSet = LongRow(currentRow(settings.keyNr).toLong, currentRow) :: currentDataSet // toLong can fail
          if (iterRow >= settings.rowSplit) {
            print("'")
            val currentFixedSet = InSort.SortLongRow(currentDataSet)
            val outFile = Tools.tempOutputFile(rootLocation, iterFile)
            Tools.writeToFile(currentFixedSet.toList, outFile, settings)
            val min = currentDataSet.head.v
            val max = currentDataSet.last.v
            sortedFiles = TempSortedFile(outFile, min, max) :: sortedFiles
            currentDataSet = Nil
            iterFile += 1
            iterRow = 0
            print(".")
          }
          currentRow = parser.parseNext()
          iterRow += 1
        }
        if (!currentDataSet.isEmpty) {
          val currentFixedSet = InSort.SortLongRow(currentDataSet)
          val outFile = Tools.tempOutputFile(rootLocation, iterFile)
          Tools.writeToFile(currentFixedSet.toList, outFile, settings)
          val min = currentDataSet.head.v
          val max = currentDataSet.last.v
          sortedFiles = TempSortedFile(outFile, min, max) :: sortedFiles
        }
      }
      case Tools.sortKeyType.stringKeyType => {
        var currentDataSet: List[StringRow] = Nil
        while (currentRow != null) {
          currentDataSet = StringRow(currentRow(settings.keyNr), currentRow) :: currentDataSet
          if (iterRow >= settings.rowSplit) {
            val currentFixedSet = InSort.SortStringRow(currentDataSet)
            val outFile = Tools.tempOutputFile(rootLocation, iterFile)
            Tools.writeToFile(currentFixedSet, outFile, settings)
            // Different SortedFile classes would be better but even better is make everything String/Long agnostic
            val min = currentDataSet.head.v(0).toLong
            val max = currentDataSet.last.v(0).toLong
            sortedFiles = TempSortedFile(outFile, min, max) :: sortedFiles
            currentDataSet = Nil
            iterFile += 1
            iterRow = 0
            print(".")
          }
          currentRow = parser.parseNext()
          iterRow += 1
        }
        if (!currentDataSet.isEmpty) {
          val currentFixedSet = InSort.SortStringRow(currentDataSet)
          val outFile = Tools.tempOutputFile(rootLocation, iterFile)
          Tools.writeToFile(currentFixedSet, outFile, settings)
          val min = currentDataSet.head.v(0).toLong
          val max = currentDataSet.last.v(0).toLong
          sortedFiles = TempSortedFile(outFile, min, max) :: sortedFiles
        }
      }
    }
    parser.stopParsing()
    print("\nFinished parsing parts\n")
    sortedFiles.reverse
  }

  /**
   * Concatenate two files
   */
  def concatSortedFiles(fileA: TempSortedFile, fileB: TempSortedFile, rootLocation: Path): TempSortedFile = {
    val nameA = fileA.file.getName().replace(".tsv", "")
    val nameB = fileB.file.getName().replace(".tsv", "")
    val outFile = new File(rootLocation.toFile, List(nameA, "-", nameB, ".tsv").reduceLeft((a, b) => a + b))
    val fileWriter = new PrintWriter(outFile)

    def writeToFile(inp: File): Unit = {
      val reader = new BufferedReader(new FileReader(inp))
      var line = reader.readLine()
      while(line != null) {
        fileWriter.println(line)
        line = reader.readLine()
      }
      reader.close()
    }

    writeToFile(fileA.file)
    writeToFile(fileB.file)
    fileWriter.flush()
    fileWriter.close()
    TempSortedFile(outFile, fileA.low, fileB.high)
  }

  def mergeAccidentalSorted(fileList: List[TempSortedFile], rootLocation: Path): List[File] = {
    /**
     * Does a single compare to the next item if it is already sorted (TODO: Optimal better combining)
     */
    @scala.annotation.tailrec
    def alreadySortedCheck(checked: List[File], toCheck: List[TempSortedFile]): List[File] = {
      toCheck match {
        case Nil => checked
        case tsA :: tsB :: rest => {
          if (tsA.high < tsB.low) {
            val newCombination = concatSortedFiles(tsA, tsB, rootLocation)
            val poppedList = toCheck diff List(tsA, tsB)
            alreadySortedCheck(checked, newCombination :: poppedList)
          } else if (tsB.high < tsA.low) {
            val newCombination = concatSortedFiles(tsB, tsA, rootLocation)
            val poppedList = toCheck diff List(tsA, tsB)
            alreadySortedCheck(checked, newCombination :: poppedList)
          } else {
            alreadySortedCheck(tsA.file :: checked, tsB :: rest)
          }
        }
        case tsA :: Nil => tsA.file :: checked
      }
    }
    alreadySortedCheck(Nil, fileList)
  } // TODO: Actually do the work

  /**
   * Merge 2 sorted files
   */
  def mergeFiles(fileA: File, fileB: File, rootLocation: Path, nr: Int, settings: ExortSetting): File = {
    val parserA = Tools.tsvParser(false)
    parserA.beginParsing(fileA)
    val parserB = Tools.tsvParser(false)
    parserB.beginParsing(fileB)
    val outFile = Tools.tempOutputFile(rootLocation, nr)
    val writer = new TsvWriter(outFile, new TsvWriterSettings)

    var currentARow: Array[String] = parserA.parseNext()
    var currentBRow: Array[String] = parserB.parseNext()
    while ((currentARow != null) && (currentBRow != null)) {
      val aVal = currentARow(settings.keyNr)
      val bVal = currentBRow(settings.keyNr)
      val swtch = settings.keyType match {
        case Tools.sortKeyType.numericKeyType => aVal.toLong - bVal.toLong
        case Tools.sortKeyType.stringKeyType => math.Ordering.String.compare(aVal, bVal)
      }
      if (swtch < 0) {
        writer.writeRow(currentARow)
        currentARow = parserA.parseNext()
      } else {
        writer.writeRow(currentBRow)
        currentBRow = parserB.parseNext()
      }
    }
    while (currentARow != null) {
      writer.writeRow(currentARow)
      currentARow = parserA.parseNext()
    }
    parserA.stopParsing()
    while (currentBRow != null) {
      writer.writeRow(currentBRow)
      currentBRow = parserB.parseNext()
    }
    parserB.stopParsing()
    // TODO: Could delete input files here to save space
    writer.close()
    outFile
  }

  def externalMergeSort(files: List[File], rootLocation: Path, setting: ExortSetting): File = {
    implicit val ec: ExecutionContext = ExecutionContext.global
    val workFiles: List[Future[File]] = files.map(Future(_))

    def processPairs(pairs: List[Future[File]], callNr: Int): List[Future[File]] = {
      val (partA, partB) = pairs.splitAt(pairs.length / 2)
      val uniqueName = callNr * 1000
      val grouped = partA.lazyZip(partB).lazyZip(0 until partA.length).toList
      grouped.map(x => Future {
        mergeFiles(
          Await.result(x._1, Duration.Inf),
          Await.result(x._2, Duration.Inf),
          rootLocation,
          uniqueName + x._3,
          setting
        )
      })
    }

    @scala.annotation.tailrec
    def processWorkFile(files: List[Future[File]], callNr: Int = 1): File = {
      if (files.length == 1) {
        return Await.result(files.head, Duration.Inf)
      }
      val outList = if((files.length % 2) != 0) {
        files.head :: processPairs(files.tail, callNr)
      } else {
        processPairs(files, callNr)
      }
      processWorkFile(outList, callNr + 1)
    }

    processWorkFile(workFiles)
  }

  def runExternalSort(setting: ExortSetting) = {
    println("Starting external sort")
    val tempDir = Tools.createTempDir()
    val sortedParts = sortParts(tempDir, setting)
    println(s"Sorted parts: ${sortedParts.length}")
    val mergeList = mergeAccidentalSorted(sortedParts, tempDir)
    println(s"Simply merged parts ${mergeList}")
    val processedFile = externalMergeSort(mergeList, tempDir, setting)
    println("Sorting done")
    Files.move(processedFile.toPath(), (new File("output.csv")).toPath)
  }
}
