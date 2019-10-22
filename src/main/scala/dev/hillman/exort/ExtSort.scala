package dev.hillman.exort

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.nio.file.{Files, Path, StandardCopyOption}

import com.univocity.parsers.tsv.{TsvWriter, TsvWriterSettings}
import dev.hillman.exort

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

sealed trait TempSortedFile {
  // Distance between two sorted files (if there is any)
  def compare(other: TempSortedFile): Double
  def file: File
}
case class LongSortedFile(vlow: Long, vhigh: Long, file: File) extends TempSortedFile {
  override def compare(other: TempSortedFile): Double = {
    other match {
      case o: LongSortedFile => {
        if (this.vhigh < o.vlow) {
          (o.vlow - this.vhigh).toDouble
        } else if (this.vlow > o.vhigh) {
          (o.vhigh - this.vlow).toDouble
        } else {
          0.0
        }
      }
      case _ => 0.0
    }
  }
}
case class DoubleSortedFile(vlow: Double, vhigh: Double, file: File) extends TempSortedFile  {
  override def compare(other: TempSortedFile): Double = {
    other match {
      case o: LongSortedFile => {
        if (this.vhigh < o.vlow) {
          (o.vlow - this.vhigh)
        } else if (this.vlow > o.vhigh) {
          (o.vhigh - this.vlow)
        } else {
          0.0
        }
      }
      case _ => 0.0
    }
  }
}
case class StringSortedFile(vlow: String, vhigh:String, file: File) extends TempSortedFile {
  override def compare(other: TempSortedFile): Double = {
    other match {
      case o: StringSortedFile => {
        if (this.vhigh < o.vlow) {
          Tools.StringDistance(this.vhigh, o.vlow)
        } else if (this.vlow > o.vhigh) {
          Tools.StringDistance(this.vlow, o.vhigh)
        } else {
          0.0
        }
      }
      case _ => 0.0
    }
  }
}
case class VarySortedFile(vlow: VaryRow, vhigh:VaryRow, file: File) extends  TempSortedFile {
  override def compare(other: TempSortedFile): Double = {
    other match {
      case o: VarySortedFile => {
        if (InSort.varyRowOrdering.compare(this.vhigh, o.vlow) > 0) {
          InSort.varyRowOrdering.distance(this.vhigh, o.vlow)
        } else if (InSort.varyRowOrdering.compare(this.vlow, o.vhigh) < 0) {
          InSort.varyRowOrdering.distance(this.vlow, o.vhigh)
        } else {
          0.0
        }
      }
      case _ => 0.0
    }
  }
}



object ExtSort {

  def sortParts(rootLocation: Path, settings: ExortSetting): List[TempSortedFile] = {
    val parser = settings.sep match {
      case '\t' => Tools.tsvParser(settings.skipHeader)
      case _ => Tools.csvParser(settings.sep, settings.skipHeader)
    }
    parser beginParsing settings.file

    def sortAsLongFile(keyCol: Int): List[TempSortedFile] = {
      var iterRow = 1
      var iterFile = 0
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[LongRow] = Nil
      while (currentRow != null) {
        currentDataSet = LongRow(currentRow(keyCol).toLong, currentRow) :: currentDataSet // toLong can fail
        if (iterRow >= settings.rowSplit) {
          print("'")
          val currentFixedSet = InSort.sortLongRow(currentDataSet)
          val outFile = Tools.tempOutputFile(rootLocation, iterFile)
          Tools.writeToFile(currentFixedSet.toList, outFile, settings)
          sortedFiles = exort.LongSortedFile(currentDataSet.head.v, currentDataSet.last.v, outFile) :: sortedFiles
          currentDataSet = Nil
          iterFile += 1
          iterRow = 0
          print(".")
        }
        currentRow = parser.parseNext()
        iterRow += 1
      }
      if (!currentDataSet.isEmpty) {
        val currentFixedSet = InSort.sortLongRow(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = exort.LongSortedFile(currentDataSet.head.v, currentDataSet.last.v, outFile) :: sortedFiles
      }
      parser.stopParsing()
      sortedFiles
    }

    def sortAsStringFile(keyCol: Int): List[TempSortedFile] = {
      var iterRow = 1
      var iterFile = 0
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[StringRow] = Nil
      while (currentRow != null) {
        currentDataSet = StringRow(currentRow(keyCol: Int), currentRow) :: currentDataSet
        if (iterRow >= settings.rowSplit) {
          val currentFixedSet = InSort.sortStringRow(currentDataSet)
          val outFile = Tools.tempOutputFile(rootLocation, iterFile)
          Tools.writeToFile(currentFixedSet.toList, outFile, settings)
          // Different SortedFile classes would be better but even better is make everything String/Long agnostic
          sortedFiles = exort.StringSortedFile(currentDataSet.head.v, currentDataSet.last.v, outFile) :: sortedFiles
          currentDataSet = Nil
          iterFile += 1
          iterRow = 0
          print(".")
        }
        currentRow = parser.parseNext()
        iterRow += 1
      }
      if (!currentDataSet.isEmpty) {
        val currentFixedSet = InSort.sortStringRow(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = exort.StringSortedFile(currentDataSet.head.v, currentDataSet.last.v, outFile) :: sortedFiles
      }
      parser.stopParsing()
      sortedFiles
    }

    def sortAsDoubleFile(keyCol: Int): List[TempSortedFile] = {
      var iterRow = 1
      var iterFile = 0
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[DoubleRow] = Nil
      while (currentRow != null) {
        currentDataSet = DoubleRow(currentRow(keyCol: Int).toDouble, currentRow) :: currentDataSet  // toDouble can fail
        if (iterRow >= settings.rowSplit) {
          val currentFixedSet = InSort.sortDoubleRow(currentDataSet)
          val outFile = Tools.tempOutputFile(rootLocation, iterFile)
          Tools.writeToFile(currentFixedSet.toList, outFile, settings)
          // Different SortedFile classes would be better but even better is make everything String/Long agnostic
          sortedFiles = exort.DoubleSortedFile(currentDataSet.head.v, currentDataSet.last.v, outFile) :: sortedFiles
          currentDataSet = Nil
          iterFile += 1
          iterRow = 0
          print(".")
        }
        currentRow = parser.parseNext()
        iterRow += 1
      }
      if (!currentDataSet.isEmpty) {
        val currentFixedSet = InSort.sortDoubleRow(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = exort.DoubleSortedFile(currentDataSet.head.v, currentDataSet.last.v, outFile) :: sortedFiles
      }
      parser.stopParsing()
      sortedFiles
    }


    val sortedFiles = {
      if(settings.keyType.length > 1) {
        println("NEED TO IMPLEMENT PARSING VARYROWs!!")
        Nil
      } else {
        settings.keyType(0) match {
          case Tools.sortKeyType.integerKeyType => sortAsLongFile(settings.keyNr(0))
          case Tools.sortKeyType.stringKeyType => sortAsStringFile(settings.keyNr(0))
          case Tools.sortKeyType.decimalKeyType => sortAsDoubleFile(settings.keyNr(0))
        }
      }
    }
    // TODO: Make the sortedFiles Futures and then wait on all of them at the end

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
    fileA match {
      case a: LongSortedFile => {
        val b = fileB.asInstanceOf[LongSortedFile]  // Danger => simply casts
        LongSortedFile(a.vlow, b.vhigh, outFile)
      }
      case a: DoubleSortedFile => {
        val b = fileB.asInstanceOf[DoubleSortedFile]
        DoubleSortedFile(a.vlow, b.vhigh, outFile)
      }
      case a: StringSortedFile => {
        val b = fileB.asInstanceOf[StringSortedFile]
        StringSortedFile(a.vlow, b.vhigh, outFile)
      }
      case a: VarySortedFile => {
        val b = fileB.asInstanceOf[VarySortedFile]
        VarySortedFile(a.vlow, b.vhigh, outFile)
      }
    }
  }

  def mergeAccidentalSorted(fileList: List[TempSortedFile], rootLocation: Path): List[File] = {
    /**
     * Looks at inter-file distance and merges (max) 2 closest files (TODO: Optimal better combining)
     */
    @scala.annotation.tailrec
    def alreadySortedCheck(checked: List[File], toCheck: List[TempSortedFile]): List[File] = {
      implicit val implicitOrdering = Ordering.Double.IeeeOrdering
      toCheck match {
        case Nil => checked
        case tsA :: Nil => tsA.file :: checked
        case tsA :: tsMore => {
          val distances = tsMore.zipWithIndex.map((x) => (tsA.compare(x._1), x._2)).sorted
          val negativeDistances = distances.filter(_._1 < 0)
          val positiveDistances = distances.filter(_._1 > 1)
          if ((negativeDistances.length > 0) && (positiveDistances.length > 0)) {
            val negCombination = concatSortedFiles(tsMore(negativeDistances.last._2), tsA, rootLocation)
            val newNegPosCombination = concatSortedFiles(negCombination, tsMore(positiveDistances.head._2), rootLocation)
            val poppedList = toCheck diff List(tsA, tsMore(negativeDistances.last._2), tsMore(positiveDistances.head._2))
            alreadySortedCheck(checked, newNegPosCombination :: poppedList)
          } else if (negativeDistances.length > 0) {
            val newCombination = concatSortedFiles(tsMore(negativeDistances.last._2), tsA, rootLocation)
            val poppedList = toCheck diff List(tsA, tsMore(negativeDistances.last._2))
            alreadySortedCheck(checked, newCombination :: poppedList)
          } else if (positiveDistances.length > 0) {
            val newCombination = concatSortedFiles(tsA, tsMore(positiveDistances.head._2), rootLocation)
            val poppedList = toCheck diff List(tsA, tsMore(positiveDistances.head._2))
            alreadySortedCheck(checked, newCombination :: poppedList)
          } else {
            alreadySortedCheck(tsA.file :: checked, tsMore)
          }
        }
      }
    }
    alreadySortedCheck(Nil, fileList)
  }

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

    /*
      Idea: Create a higher order function which when given a setting creates a new function
      that returns the right object when given an Array of Strings
      Then re-use that for the sortParts and here (still a better solution is here to make it more generic)
     */
    var currentARow: Array[String] = parserA.parseNext()
    var currentBRow: Array[String] = parserB.parseNext()
    if(settings.keyType.length > 1) {
      println("NEED TO IMPLEMENT PARSING VARYROWs!!")
    } else {
      val keyCol = settings.keyNr(0)
      settings.keyType(0) match {
        case Tools.sortKeyType.integerKeyType => {
          while ((currentARow != null) && (currentBRow != null)) {
            val typedARow = LongRow(currentARow(keyCol).toLong, Array())
            val typedBRow = LongRow(currentBRow(keyCol).toLong, Array())
            val swtch = LongRowOrdering.compare(typedARow, typedBRow)
            if (swtch < 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
            }
          }
        }
        case Tools.sortKeyType.stringKeyType => {
          while ((currentARow != null) && (currentBRow != null)) {
            val typedARow = StringRow(currentARow(keyCol), Array())
            val typedBRow = StringRow(currentBRow(keyCol), Array())
            val swtch = StringRowOrdering.compare(typedARow, typedBRow)
            if (swtch < 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
            }
          }
        }
        case Tools.sortKeyType.decimalKeyType => {
          while ((currentARow != null) && (currentBRow != null)) {
            val typedARow = DoubleRow(currentARow(keyCol).toDouble, Array())
            val typedBRow = DoubleRow(currentBRow(keyCol).toDouble, Array())
            val swtch = DoubleRowOrdering.compare(typedARow, typedBRow)
            if (swtch < 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
            }
          }
        }
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
      // Every round merges has a unique group-name and then lazyzip unique file-ids
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
        // If there is a single file left, materialize result and return
        return Await.result(files.head, Duration.Inf)
      }
      val outList = if((files.length % 2) != 0) {
        // If total is uneven, first join an even amount
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
    Files.move(processedFile.toPath(), (new File("output.csv")).toPath, StandardCopyOption.REPLACE_EXISTING)
    Tools.cleanDirectory(tempDir)
  }
}
