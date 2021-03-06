package dev.hillman.exort

import java.io.{BufferedReader, File, FileReader, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import com.univocity.parsers.tsv.{TsvWriter, TsvWriterSettings}
import dev.hillman.exort

import scala.Console.println
import scala.collection.SeqView.Reverse
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Sorting

object ExtSort {

  def sortParts(rootLocation: Path,
                settings: ExortSetting): List[TempSortedFile] = {
    val parser = settings.sep match {
      case '\t' => Tools.tsvParser(settings.skipHeader)
      case _    => Tools.csvParser(settings.sep, settings.skipHeader)
    }

    var iterFile = 0

    def sortAsLongFile(keyCol: Int, reverse: Boolean): List[TempSortedFile] = {
      var iterRow = 1
      var originalRowNr = 1
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[LongRow] = Nil
      while (currentRow != null) {
        if (currentRow.length >= keyCol + 1) {
          val columnVal = currentRow(keyCol)
          val longVal: Option[Long] = if (columnVal != null) {
            columnVal.toLongOption
          } else {
            Option.empty
          }
          if (longVal.isDefined) {
            currentDataSet = LongRow(longVal.get, currentRow) :: currentDataSet // toLong can fail
            if (iterRow >= settings.rowSplit) {
              print("'")
              val currentFixedSet = InSort.sortLongRow(currentDataSet, reverse)
              val outFile = Tools.tempOutputFile(rootLocation, iterFile)
              Tools.writeToFile(currentFixedSet.toList, outFile, settings)
              sortedFiles = if (reverse) {
                exort.LongSortedFile(currentFixedSet.last.v,
                                     currentFixedSet.head.v,
                                     outFile) :: sortedFiles
              } else {
                exort.LongSortedFile(currentFixedSet.head.v,
                                     currentFixedSet.last.v,
                                     outFile) :: sortedFiles
              }
              currentDataSet = Nil
              iterFile += 1
              iterRow = 0
              print(".")
            }
          } else {
            System.err.println(
              s"Could not read long column on $originalRowNr\n line: ${currentRow
                .mkString(settings.sep.toString)}")
          }
        } else {
          System.err.println(
            s"Could not read the line on $originalRowNr\n line: ${currentRow
              .mkString(settings.sep.toString)}")
        }
        currentRow = parser.parseNext()
        iterRow += 1
        originalRowNr += 1
      }
      if (currentDataSet.nonEmpty) {
        val currentFixedSet = InSort.sortLongRow(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = if (reverse) {
          exort.LongSortedFile(currentFixedSet.last.v,
                               currentFixedSet.head.v,
                               outFile) :: sortedFiles
        } else {
          exort.LongSortedFile(currentFixedSet.head.v,
                               currentFixedSet.last.v,
                               outFile) :: sortedFiles
        }
      }
      parser.stopParsing()
      sortedFiles
    }

    def sortAsStringFile(keyCol: Int,
                         reverse: Boolean): List[TempSortedFile] = {
      var iterRow = 1
      var originalRowNr = 1
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[StringRow] = Nil
      while (currentRow != null) {
        if (currentRow.length >= keyCol + 1) {
          val columnVal = currentRow(keyCol)
          if (columnVal != null) {
            currentDataSet = StringRow(columnVal.toString, currentRow) :: currentDataSet
            if (iterRow >= settings.rowSplit) {
              val currentFixedSet =
                InSort.sortStringRow(currentDataSet, reverse)
              val outFile = Tools.tempOutputFile(rootLocation, iterFile)
              Tools.writeToFile(currentFixedSet.toList, outFile, settings)
              // Different SortedFile classes would be better but even better is make everything String/Long agnostic
              sortedFiles = if (reverse) {
                exort.StringSortedFile(currentFixedSet.last.v,
                                       currentFixedSet.head.v,
                                       outFile) :: sortedFiles
              } else {
                exort.StringSortedFile(currentFixedSet.head.v,
                                       currentFixedSet.last.v,
                                       outFile) :: sortedFiles
              }
              currentDataSet = Nil
              iterFile += 1
              iterRow = 0
              print(".")
            }
          } else {
            System.err.println(
              s"There was no string value in the sort column on $originalRowNr\n line: ${currentRow
                .mkString(settings.sep.toString)}")
          }
        } else {
          System.err.println(
            s"Could not read the line on $originalRowNr\n line: ${currentRow
              .mkString(settings.sep.toString)}")
        }
        currentRow = parser.parseNext()
        iterRow += 1
        originalRowNr += 1
      }
      if (currentDataSet.nonEmpty) {
        val currentFixedSet = InSort.sortStringRow(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = if (reverse) {
          exort.StringSortedFile(currentFixedSet.last.v,
                                 currentFixedSet.head.v,
                                 outFile) :: sortedFiles
        } else {
          exort.StringSortedFile(currentFixedSet.head.v,
                                 currentFixedSet.last.v,
                                 outFile) :: sortedFiles
        }
      }
      parser.stopParsing()
      sortedFiles
    }

    def sortAsDoubleFile(keyCol: Int,
                         reverse: Boolean): List[TempSortedFile] = {
      var iterRow = 1
      var originalRowNr = 1
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[DoubleRow] = Nil
      while (currentRow != null) {
        if (currentRow.length >= keyCol + 1) {
          val columnVal = currentRow(keyCol)
          val doubleVal: Option[Double] = if (columnVal != null) {
            columnVal.toDoubleOption
          } else {
            Option.empty
          }
          if (doubleVal.isDefined) {
            currentDataSet = DoubleRow(doubleVal.get, currentRow) :: currentDataSet // toDouble can fail
            if (iterRow >= settings.rowSplit) {
              val currentFixedSet =
                InSort.sortDoubleRow(currentDataSet, reverse)
              val outFile = Tools.tempOutputFile(rootLocation, iterFile)
              Tools.writeToFile(currentFixedSet.toList, outFile, settings)
              // Different SortedFile classes would be better but even better is make everything String/Long agnostic
              sortedFiles = if (reverse) {
                exort.DoubleSortedFile(currentFixedSet.last.v,
                                       currentFixedSet.head.v,
                                       outFile) :: sortedFiles
              } else {
                exort.DoubleSortedFile(currentFixedSet.head.v,
                                       currentFixedSet.last.v,
                                       outFile) :: sortedFiles
              }
              currentDataSet = Nil
              iterFile += 1
              originalRowNr += 1
              iterRow = 0
              print(".")
            }
          } else {
            System.err.println(
              s"Could not read double column on $originalRowNr\n line: ${currentRow
                .mkString(settings.sep.toString)}")
          }
        } else {
          System.err.println(
            s"Could not read the line on $originalRowNr\n line: ${currentRow
              .mkString(settings.sep.toString)}")
        }
        currentRow = parser.parseNext()
        iterRow += 1
      }
      if (currentDataSet.nonEmpty) {
        val currentFixedSet = InSort.sortDoubleRow(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = if (reverse) {
          exort.DoubleSortedFile(currentFixedSet.last.v,
                                 currentFixedSet.head.v,
                                 outFile) :: sortedFiles
        } else {
          exort.DoubleSortedFile(currentFixedSet.head.v,
                                 currentFixedSet.last.v,
                                 outFile) :: sortedFiles
        }
      }
      parser.stopParsing()
      sortedFiles
    }

    def sortAsVaryRowFile(): List[TempSortedFile] = {
      var iterRow = 1
      var originalRowNr = 1
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[VaryRow] = Nil
      while (currentRow != null) {
        val newRow = Tools.convertToVaryRow(currentRow, settings)
        newRow match {
          case Left(value) =>
            System.err.println(
              value +
                s" on line $originalRowNr\n line: ${currentRow
                  .mkString(settings.sep.toString)}")
          case Right(value) => {
            currentDataSet = value :: currentDataSet
            if (iterRow >= settings.rowSplit) {
              val currentFixedSet = InSort.sortVaryRow(currentDataSet)
              val outFile = Tools.tempOutputFile(rootLocation, iterFile)
              Tools.writeToFile(currentFixedSet.toList, outFile, settings)
              // Different SortedFile classes would be better but even better is make everything String/Long agnostic
              sortedFiles = exort.VarySortedFile(currentFixedSet.head,
                                                 currentFixedSet.last,
                                                 outFile) :: sortedFiles
              currentDataSet = Nil
              iterFile += 1
              iterRow = 0
              print(".")
            }
          }
        }
        currentRow = parser.parseNext()
        iterRow += 1
        originalRowNr += 1
      }
      if (currentDataSet.nonEmpty) {
        val currentFixedSet = InSort.sortVaryRow(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = exort.VarySortedFile(currentDataSet.head,
                                           currentDataSet.last,
                                           outFile) :: sortedFiles
      }
      parser.stopParsing()
      sortedFiles
    }

    def sortAsComplexVaryRow(): List[TempSortedFile] = {
      var iterRow = 1
      var originalRowNr = 1
      var sortedFiles: List[TempSortedFile] = Nil
      var currentRow: Array[String] = parser.parseNext()
      var currentDataSet: List[VaryRowComplex] = Nil
      while (currentRow != null) {
        val newRow = Tools.convertToComplexVaryRow(currentRow, settings)
        newRow match {
          case Left(value) =>
            System.err.println(
              value +
                s" on line $originalRowNr\n line: ${currentRow
                  .mkString(settings.sep.toString)}")
          case Right(value) => {
            currentDataSet = value :: currentDataSet
            if (iterRow >= settings.rowSplit) {
              val currentFixedSet = InSort.sortVaryRowComplex(currentDataSet)
              val outFile = Tools.tempOutputFile(rootLocation, iterFile)
              Tools.writeToFile(currentFixedSet.toList, outFile, settings)
              sortedFiles = exort.ComplexVarySortedFile(currentFixedSet.head,
                                                        currentFixedSet.last,
                                                        outFile) :: sortedFiles
              currentDataSet = Nil
              iterFile += 1
              iterRow = 0
              print(".")
            }
          }
        }
        currentRow = parser.parseNext()
        iterRow += 1
        originalRowNr += 1
      }
      if (currentDataSet.nonEmpty) {
        val currentFixedSet = InSort.sortVaryRowComplex(currentDataSet)
        val outFile = Tools.tempOutputFile(rootLocation, iterFile)
        Tools.writeToFile(currentFixedSet.toList, outFile, settings)
        sortedFiles = exort.ComplexVarySortedFile(currentDataSet.head,
                                                  currentDataSet.last,
                                                  outFile) :: sortedFiles
      }
      parser.stopParsing()
      sortedFiles
    }

    val sortedFiles: List[TempSortedFile] =
      settings.files.flatMap((inputFile: File) => {
        parser.beginParsing(inputFile)

        val partSortedFiles = if (settings.complexSort) {
          sortAsComplexVaryRow()
        } else if (settings.keyType.length > 1) {
          sortAsVaryRowFile()
        } else {
          settings.keyType(0) match {
            case Tools.sortKeyType.integerKeyType =>
              sortAsLongFile(settings.keyNr(0), false)
            case Tools.sortKeyType.stringKeyType =>
              sortAsStringFile(settings.keyNr(0), false)
            case Tools.sortKeyType.decimalKeyType =>
              sortAsDoubleFile(settings.keyNr(0), false)
            case Tools.sortKeyType.integerNegKeyType =>
              sortAsLongFile(settings.keyNr(0), true)
            case Tools.sortKeyType.stringNegKeyType =>
              sortAsStringFile(settings.keyNr(0), true)
            case Tools.sortKeyType.decimalNegKeyType =>
              sortAsDoubleFile(settings.keyNr(0), true)
          }
        }
        partSortedFiles.reverse
      })

    println("\nFinished parsing & sorting parts\n")
    sortedFiles
  }

  /**
    * Concatenate two files (Warning: Does no checks of the data!)
    */
  def concatSortedFiles(fileA: TempSortedFile,
                        fileB: TempSortedFile,
                        rootLocation: Path): TempSortedFile = {
    val nameA = fileA.file.getName().replace(".tsv", "")
    val nameB = fileB.file.getName().replace(".tsv", "")
    val outFile = new File(
      rootLocation.toFile,
      List(nameA, "-", nameB, ".tsv").reduceLeft((a, b) => a + b))
    val fileWriter = new PrintWriter(outFile)

    // Take other file and write contents to single file (fileWriter)
    def writeToFile(inp: File): Unit = {
      val reader = new BufferedReader(new FileReader(inp))
      var line = reader.readLine()
      while (line != null) {
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
        val b = fileB.asInstanceOf[LongSortedFile] // Danger => simply casts
        LongSortedFile(a.vlow, b.vhigh, outFile) // TODO: Check neg logic still holds
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
        VarySortedFile(a.vstart, b.vend, outFile)
      }
      case a: ComplexVarySortedFile => {
        val b = fileB.asInstanceOf[ComplexVarySortedFile]
        ComplexVarySortedFile(a.vstart, b.vend, outFile)
      }
    }
  }

  def mergeAccidentalSorted(fileList: List[TempSortedFile],
                            rootLocation: Path): List[File] = {

    /**
      * Looks at inter-file distance and merges (max) 2 closest files (TODO: Optimal better combining)
      */
    @scala.annotation.tailrec
    def alreadySortedCheck(checked: List[File],
                           toCheck: List[TempSortedFile]): List[File] = {
      // TODO: Make sure that reverse is passed along if set
      implicit val implicitOrdering = Ordering.Double.IeeeOrdering
      toCheck match {
        case Nil        => checked
        case tsA :: Nil => tsA.file :: checked
        case tsA :: tsMore => {
          // More than 1 other file => split them up in before and after the current (toCheck == tsA)
          val (beforeDistancesIdxs, afterDistancesIdxs) = tsA match {
            case tsAL: LongSortedFile => {
              val otherFiles =
                tsMore.asInstanceOf[List[LongSortedFile]].zipWithIndex
              val distances = otherFiles.map(otherFile =>
                (tsAL.distance(otherFile._1), otherFile._2))
              val sortedDistances = Sorting.stableSort(
                distances,
                (valIdx1: (Long, Int), valIdx2: (Long, Int)) =>
                  valIdx1._1 < valIdx2._1)
              (sortedDistances
                 .filter(valIdx => valIdx._1 > 0L)
                 .map(_._2), // res > 0 -> tsA after
               sortedDistances
                 .filter(valIdx => valIdx._1 < 0L)
                 .map(_._2)) // res < 0 -> tsA before
            }
            case tsAD: DoubleSortedFile => {
              val otherFiles =
                tsMore.asInstanceOf[List[DoubleSortedFile]].zipWithIndex
              val distances = otherFiles.map(otherFile =>
                (tsAD.distance(otherFile._1), otherFile._2))
              val sortedDistances = Sorting.stableSort(
                distances,
                (valIdx1: (Double, Int), valIdx2: (Double, Int)) =>
                  valIdx1._1 < valIdx2._1)
              (sortedDistances
                 .filter(valIdx => valIdx._1 > 0.0)
                 .map(_._2), // res > 0 -> tsA after
               sortedDistances
                 .filter(valIdx => valIdx._1 < 0.0)
                 .map(_._2)) // res < 0 -> tsA before
            }
            case tsAS: StringSortedFile => {
              val otherFiles =
                tsMore.asInstanceOf[List[StringSortedFile]].zipWithIndex
              val distances = otherFiles.map(otherFile =>
                (tsAS.distance(otherFile._1), otherFile._2))
              val sortedDistances = Sorting.stableSort(
                distances,
                (valIdx1: (List[Int], Int), valIdx2: (List[Int], Int)) =>
                  FileSort.lt(valIdx1._1, valIdx2._1))
              (sortedDistances
                 .filter(valIdx => FileSort.gt(valIdx._1, Nil))
                 .map(_._2), // res > 0 -> tsA after
               sortedDistances
                 .filter(valIdx => FileSort.lt(valIdx._1, Nil))
                 .map(_._2)) // res < 0 -> tsA before
            }
            case tsAV: VarySortedFile => {
              val otherFiles =
                tsMore.asInstanceOf[List[VarySortedFile]].zipWithIndex
              val distances = otherFiles.map(otherFile =>
                (tsAV.distance(otherFile._1), otherFile._2))
              val sortedDistances = Sorting.stableSort(
                distances,
                (valIdx1: (List[Double], Int), valIdx2: (List[Double], Int)) =>
                  FileSort.lt(valIdx1._1, valIdx2._1))
              (sortedDistances
                 .filter(valIdx => FileSort.gt(valIdx._1, Nil))
                 .map(_._2), // res > 0 -> tsA after
               sortedDistances
                 .filter(valIdx => FileSort.lt(valIdx._1, Nil))
                 .map(_._2)) // res < 0 -> tsA before
            }
            case tsAVC: ComplexVarySortedFile => {
              val otherFiles =
                tsMore.asInstanceOf[List[ComplexVarySortedFile]].zipWithIndex
              val distances = otherFiles.map(otherFile =>
                (tsAVC.distance(otherFile._1), otherFile._2))
              val sortedDistances = Sorting.stableSort(
                distances,
                (valIdx1: (List[BigDecimal], Int),
                 valIdx2: (List[BigDecimal], Int)) =>
                  FileSort.lt(valIdx1._1, valIdx2._1))
              (sortedDistances
                 .filter(valIdx => FileSort.gt(valIdx._1, Nil))
                 .map(_._2), // res > 0 -> tsA after
               sortedDistances
                 .filter(valIdx => FileSort.lt(valIdx._1, Nil))
                 .map(_._2)) // res < 0 -> tsA before
            }
          }

          if (beforeDistancesIdxs.nonEmpty && afterDistancesIdxs.nonEmpty) {
            val negCombination = concatSortedFiles(
              tsMore(beforeDistancesIdxs.last),
              tsA,
              rootLocation)
            val newNegPosCombination = concatSortedFiles(
              negCombination,
              tsMore(afterDistancesIdxs.head),
              rootLocation)
            val poppedList = toCheck diff List(tsA,
                                               tsMore(beforeDistancesIdxs.last),
                                               tsMore(afterDistancesIdxs.head))
            alreadySortedCheck(checked, newNegPosCombination :: poppedList)
          } else if (beforeDistancesIdxs.nonEmpty) {
            val newCombination = concatSortedFiles(
              tsMore(beforeDistancesIdxs.last),
              tsA,
              rootLocation)
            val poppedList = toCheck diff List(tsA,
                                               tsMore(beforeDistancesIdxs.last))
            alreadySortedCheck(checked, newCombination :: poppedList)
          } else if (afterDistancesIdxs.nonEmpty) {
            val newCombination = concatSortedFiles(
              tsA,
              tsMore(afterDistancesIdxs.head),
              rootLocation)
            val poppedList = toCheck diff List(tsA,
                                               tsMore(afterDistancesIdxs.head))
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
  def mergeFiles(fileA: File,
                 fileB: File,
                 rootLocation: Path,
                 nr: Int,
                 settings: ExortSetting): File = {
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
    var compareActive = true
    // At this point it should always be readable lines, but could add more error checking here later if needed
    if (settings.complexSort) {
      var typedARow =
        Tools
          .convertToComplexVaryRow(currentARow, settings, addSource = false)
          .toOption
          .get
      var typedBRow =
        Tools
          .convertToComplexVaryRow(currentBRow, settings, addSource = false)
          .toOption
          .get
      while (compareActive) {
        if (ComplexVROrdering.compare(typedARow, typedBRow) < 0) {
          writer.writeRow(currentARow)
          currentARow = parserA.parseNext()
          if (currentARow != null) {
            typedARow = Tools
              .convertToComplexVaryRow(currentARow, settings, addSource = false)
              .toOption
              .get
          } else {
            compareActive = false
          }
        } else {
          writer.writeRow(currentBRow)
          currentBRow = parserB.parseNext()
          if (currentBRow != null) {
            typedBRow = Tools
              .convertToComplexVaryRow(currentBRow, settings, addSource = false)
              .toOption
              .get
          } else {
            compareActive = false
          }
        }
      }
    } else if (settings.keyType.length > 1) {
      var typedARow =
        Tools
          .convertToVaryRow(currentARow, settings, addSource = false)
          .toOption
          .get
      var typedBRow =
        Tools
          .convertToVaryRow(currentBRow, settings, addSource = false)
          .toOption
          .get
      while (compareActive) {
        if (VaryRowOrdering.compare(typedARow, typedBRow) < 0) {
          writer.writeRow(currentARow)
          currentARow = parserA.parseNext()
          if (currentARow != null) {
            typedARow = Tools
              .convertToVaryRow(currentARow, settings, addSource = false)
              .toOption
              .get
          } else {
            compareActive = false
          }
        } else {
          writer.writeRow(currentBRow)
          currentBRow = parserB.parseNext()
          if (currentBRow != null) {
            typedBRow = Tools
              .convertToVaryRow(currentBRow, settings, addSource = false)
              .toOption
              .get
          } else {
            compareActive = false
          }
        }
      }
    } else {
      val keyCol = settings.keyNr.head
      settings.keyType.head match {
        case Tools.sortKeyType.integerKeyType => {
          var typedARow = LongRow(currentARow(keyCol).toLong, Array())
          var typedBRow = LongRow(currentBRow(keyCol).toLong, Array())
          while (compareActive) {
            if (LongRowOrdering.compare(typedARow, typedBRow) < 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
              if (currentARow != null) {
                typedARow = LongRow(currentARow(keyCol).toLong, Array())
              } else {
                compareActive = false
              }
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
              if (currentBRow != null) {
                typedBRow = LongRow(currentBRow(keyCol).toLong, Array())
              } else {
                compareActive = false
              }
            }
          }
        }
        case Tools.sortKeyType.integerNegKeyType => {
          var typedARow = LongRow(currentARow(keyCol).toLong, Array())
          var typedBRow = LongRow(currentBRow(keyCol).toLong, Array())
          while (compareActive) {
            if (LongRowOrdering.compare(typedARow, typedBRow) > 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
              if (currentARow != null) {
                typedARow = LongRow(currentARow(keyCol).toLong, Array())
              } else {
                compareActive = false
              }
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
              if (currentBRow != null) {
                typedBRow = LongRow(currentBRow(keyCol).toLong, Array())
              } else {
                compareActive = false
              }
            }
          }
        }
        case Tools.sortKeyType.stringKeyType => {
          var typedARow = StringRow(currentARow(keyCol), Array())
          var typedBRow = StringRow(currentBRow(keyCol), Array())
          while (compareActive) {
            if (StringRowOrdering.compare(typedARow, typedBRow) < 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
              if (currentARow != null) {
                typedARow = StringRow(currentARow(keyCol), Array())
              } else {
                compareActive = false
              }
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
              if (currentBRow != null) {
                typedBRow = StringRow(currentBRow(keyCol), Array())
              } else {
                compareActive = false
              }
            }
          }
        }
        case Tools.sortKeyType.stringNegKeyType => {
          var typedARow = StringRow(currentARow(keyCol), Array())
          var typedBRow = StringRow(currentBRow(keyCol), Array())
          while (compareActive) {
            if (StringRowOrdering.compare(typedARow, typedBRow) > 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
              if (currentARow != null) {
                typedARow = StringRow(currentARow(keyCol), Array())
              } else {
                compareActive = false
              }
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
              if (currentBRow != null) {
                typedBRow = StringRow(currentBRow(keyCol), Array())
              } else {
                compareActive = false
              }
            }
          }
        }
        case Tools.sortKeyType.decimalKeyType => {
          var typedARow = DoubleRow(currentARow(keyCol).toDouble, Array())
          var typedBRow = DoubleRow(currentBRow(keyCol).toDouble, Array())
          while (compareActive) {
            if (DoubleRowOrdering.compare(typedARow, typedBRow) < 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
              if (currentARow != null) {
                typedARow = DoubleRow(currentARow(keyCol).toDouble, Array())
              } else {
                compareActive = false
              }
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
              if (currentBRow != null) {
                typedBRow = DoubleRow(currentBRow(keyCol).toDouble, Array())
              } else {
                compareActive = false
              }
            }
          }
        }
        case Tools.sortKeyType.decimalNegKeyType => {
          var typedARow = DoubleRow(currentARow(keyCol).toDouble, Array())
          var typedBRow = DoubleRow(currentBRow(keyCol).toDouble, Array())
          while (compareActive) {
            if (DoubleRowOrdering.compare(typedARow, typedBRow) > 0) {
              writer.writeRow(currentARow)
              currentARow = parserA.parseNext()
              if (currentARow != null) {
                typedARow = DoubleRow(currentARow(keyCol).toDouble, Array())
              } else {
                compareActive = false
              }
            } else {
              writer.writeRow(currentBRow)
              currentBRow = parserB.parseNext()
              if (currentBRow != null) {
                typedBRow = DoubleRow(currentBRow(keyCol).toDouble, Array())
              } else {
                compareActive = false
              }
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

  def externalMergeSort(files: List[File],
                        rootLocation: Path,
                        setting: ExortSetting): File = {
    implicit val ec: ExecutionContext = ExecutionContext.global
    val workFiles: List[Future[File]] = files.map(Future(_))

    def processPairs(pairs: List[Future[File]],
                     callNr: Int): List[Future[File]] = {
      val (partA, partB) = pairs.splitAt(pairs.length / 2)
      // Every round merges has a unique group-name and then lazyzip unique file-ids
      val uniqueName = callNr * 1000
      val grouped = partA.lazyZip(partB).lazyZip(partA.indices).toList
      grouped.map(x =>
        Future {
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
      val outList = if ((files.length % 2) != 0) {
        // If total is uneven, first join an even amount
        files.head :: processPairs(files.tail, callNr)
      } else {
        processPairs(files, callNr)
      }
      processWorkFile(outList, callNr + 1)
    }

    processWorkFile(workFiles)
  }

  /**
    * Does the complete external sorting according to the settings
    */
  def runExternalSort(setting: ExortSetting): Unit = {
    println("Starting external sort")
    val tempDir = Tools.createTempDir()
    val sortedParts = sortParts(tempDir, setting)
    println(s"Sorted parts: ${sortedParts.length}")
    val mergeList = mergeAccidentalSorted(sortedParts, tempDir)
    println(s"Merged parts (compare-less): ${mergeList.length}")
    val processedFile = externalMergeSort(mergeList, tempDir, setting)
    println("Sorting done")

    Files.move(processedFile.toPath,
               Paths.get(setting.outFileName),
               StandardCopyOption.REPLACE_EXISTING)
    Tools.cleanDirectory(tempDir)
  }
}
