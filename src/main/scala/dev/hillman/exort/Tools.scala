package dev.hillman.exort

import java.io.File
import java.nio.file.{Files, Path}

import scala.jdk.StreamConverters._
import com.univocity.parsers.csv.{CsvParser, CsvParserSettings}
import com.univocity.parsers.tsv.{TsvParser, TsvParserSettings, TsvWriter, TsvWriterSettings}
import Tools.sortKeyType

import scala.Option
import scala.util.Random

object Tools {
  object sortKeyType extends Enumeration {
    type sortKeyType = Value
    val decimalKeyType, decimalNegKeyType, integerKeyType, integerNegKeyType, stringKeyType,
        stringNegKeyType, stringLowerCaseKeyType, stringLowerCaseNegKeyType = Value
  }

  /**
    * Create a new csv parser
    * @param sep the separator character
    * @param hasHeader Is there a header which should be skipped
    * @return new parser object
    */
  def csvParser(sep: Char, hasHeader: Boolean): CsvParser = {
    val settings = new CsvParserSettings()
    val fmt = settings.getFormat()
    fmt.setDelimiter(sep)
    settings.getFormat.setLineSeparator("\n")
    settings.setHeaderExtractionEnabled(hasHeader)
    settings.setSkipEmptyLines(true)
    new CsvParser(settings)
  }

  /**
    * Create a new Tsv-parser
    * @param hasHeader Is there a header which should be skipped
    * @return new parser object
    */
  def tsvParser(hasHeader: Boolean): TsvParser = {
    val settings = new TsvParserSettings()
    settings.getFormat.setLineSeparator("\n")
    settings.setHeaderExtractionEnabled(hasHeader)
    settings.setSkipEmptyLines(true)
    new TsvParser(settings)
  }

  /**
    * Create a directory in temp for storing the data  for intermediate steps
    * @return Path to the temporary directory
    */
  def createTempDir(): Path = {
    val tempPrefix = "exsort_" + Random.alphanumeric
      .take(10)
      .foldLeft("")((x: String, y: Char) => x + y)
    Files.createTempDirectory(tempPrefix)
  }

  /**
    * Create a new (temp) File with name based on a number
    * @param location Path to the new file's location
    * @param nr number used for the name of the file
    * @return new File object
    */
  def tempOutputFile(location: Path, nr: Int = 0): File =
    new File(location.toFile, s"p${nr}.tsv")

  def endOutputFileName(oldFileName: String): String = {
    val outFileNameParts = oldFileName.split('.')
    val baseName = outFileNameParts.slice(0, outFileNameParts.length - 1).mkString("_")
    baseName + "_sorted.tsv"
  }

  /**
    * Write a list of rows to a tsv-file
    */
  def writeToFile(data: Iterable[SortableRow], fileLoc: File, settings: ExortSetting): Unit = {
    val writer = new TsvWriter(fileLoc, new TsvWriterSettings)
    val outArray: Iterable[Array[String]] =
      data.map((x: SortableRow) => x.getContent)
    outArray.foreach(writer.writeRow(_))
    writer.close()
  }

  /**
    * Alphabetical distance (NO edit distance)
    */
  @scala.annotation.tailrec
  private def StringDistancePerChar(a: String, b: String, prevDistances: List[Int]): List[Int] = {
    if ((a.isEmpty) && (b.isEmpty)) {
      return prevDistances.reverse
    } else if (a.isEmpty) {
      return (-b.head.toInt :: prevDistances).reverse
    } else if (b.isEmpty) {
      return (a.head.toInt :: prevDistances).reverse
    }
    val diff = a.head - b.head
    StringDistancePerChar(a.tail, b.tail, diff :: prevDistances)
  }

  /**
    * Alphabetical distance with case equality
    */
  def StringDistance(a: String, b: String): List[Int] =
    StringDistancePerChar(a, b, Nil)

  /**
    * Delete all files at a location
    * @param location the path to this location
    */
  def cleanDirectory(location: Path): Unit = {
    // Clean the directory and all files within here
    println(s"Deleting files at $location")
    val filePathList = Files.list(location).toScala(Seq)
    filePathList.map((x: Path) => x.toFile.delete())
  }

  /**
    * Create a new VaryRow based based on a string-array and the settings
    * @param inSource raw separated column data
    * @param setting settings for interpreting the file
    * @param addSource Store the original string array in the object
    * @return a new VaryRow object
    */
  def convertToVaryRow(
      inSource: Array[String],
      setting: ExortSetting,
      addSource: Boolean = true
  ): Either[String, VaryRow] = {

    val inSourceSafe = inSource.lift
    def getSafe(idx: Int): Option[String] =
      inSourceSafe(idx).flatMap(s =>
        s match {
          case null        => Option.empty
          case anS: String => Option(anS)
        }
      )

    @scala.annotation.tailrec
    def createRow(idx: Int, outRow: VaryRow): Either[String, VaryRow] = {
      if (idx >= setting.keyType.length) {
        return Right(outRow)
      }
      setting.keyType(idx) match {
        case sortKeyType.integerKeyType => {
          val longVal = getSafe(setting.keyNr(idx)).flatMap(_.toLongOption)
          if (longVal.isEmpty) {
            return Left(s"Could not read longval at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v3 = outRow.v3 :+ longVal.get)
          createRow(idx + 1, newRow)
        }
        case sortKeyType.integerNegKeyType => {
          val longVal = getSafe(setting.keyNr(idx)).flatMap(_.toLongOption)
          if (longVal.isEmpty) {
            return Left(s"Could not read longval at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v3 = outRow.v3 :+ longVal.get)
          createRow(idx + 1, newRow)
        }
        case sortKeyType.decimalKeyType => {
          val doubleVal = getSafe(setting.keyNr(idx)).flatMap(_.toDoubleOption)
          if (doubleVal.isEmpty) {
            return Left(s"Could not read doubleval at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v2 = outRow.v2 :+ doubleVal.get)
          createRow(idx + 1, newRow)
        }
        case sortKeyType.decimalNegKeyType => {
          val doubleVal = getSafe(setting.keyNr(idx)).flatMap(_.toDoubleOption)
          if (doubleVal.isEmpty) {
            return Left(s"Could not read doubleval at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v2 = outRow.v2 :+ doubleVal.get)
          createRow(idx + 1, newRow)
        }
        case sortKeyType.stringKeyType | sortKeyType.stringNegKeyType => {
          val strVal = getSafe(setting.keyNr(idx))
          if (strVal.isEmpty) {
            return Left(s"Could not read stringval at col ${setting.keyNr(idx)}")
          }
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ strVal.get)
          createRow(idx + 1, newRow)
        }
        case sortKeyType.stringLowerCaseKeyType | sortKeyType.stringLowerCaseNegKeyType => {
          val strVal = getSafe(setting.keyNr(idx)).map(_.toLowerCase)
          if (strVal.isEmpty) {
            return Left(s"Could not read stringval at col ${setting.keyNr(idx)}")
          }
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ strVal.get)
          createRow(idx + 1, newRow)
        }
        case _ => Left(s"Unknown sortKeyType: ${setting.keyType(idx)}")
      }
    }
    val emptyRow = if (addSource) {
      VaryRow(Nil, Nil, Nil, setting.keyType, inSource)
    } else {
      VaryRow(Nil, Nil, Nil, setting.keyType, Array[String]())
    }
    createRow(0, emptyRow)
  }

  /**
    * Create a new VaryRowComplex based based on a string-array and the settings
    * @param inSource raw separated column data
    * @param setting settings for interpreting the file
    * @param addSource Store the original string array in the object
    * @return a new VaryRowComplex object
    */
  def convertToComplexVaryRow(
      inSource: Array[String],
      setting: ExortSetting,
      addSource: Boolean = true
  ): Either[String, VaryRowComplex] = {

    @scala.annotation.tailrec
    def createComplexRow(idx: Int, outRow: VaryRowComplex): Either[String, VaryRowComplex] = {
      if (idx >= setting.keyType.length) {
        return Right(outRow)
      }
      setting.keyType(idx) match {
        case sortKeyType.integerKeyType => {
          val bintVal =
            try {
              Option(BigInt(inSource(setting.keyNr(idx))))
            } catch {
              case np: java.lang.NullPointerException => Option.empty
              case e: java.lang.NumberFormatException => Option.empty
            }
          if (bintVal.isEmpty) {
            return Left(s"Could not read bigint at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v3 = outRow.v3 :+ bintVal.get)
          createComplexRow(idx + 1, newRow)
        }
        case sortKeyType.integerNegKeyType => {
          val bintVal =
            try {
              Option(BigInt(inSource(setting.keyNr(idx))))
            } catch {
              case np: java.lang.NullPointerException => Option.empty
              case e: java.lang.NumberFormatException => Option.empty
            }
          if (bintVal.isEmpty) {
            return Left(s"Could not read bigint at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v3 = outRow.v3 :+ bintVal.get)
          createComplexRow(idx + 1, newRow)
        }
        case sortKeyType.decimalKeyType => {
          val bdecVal =
            try {
              Option(BigDecimal(inSource(setting.keyNr(idx))))
            } catch {
              case np: java.lang.NullPointerException => Option.empty
              case e: java.lang.NumberFormatException => Option.empty
            }
          if (bdecVal.isEmpty) {
            return Left(s"Could not read bigdecimal at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v2 = outRow.v2 :+ bdecVal.get)
          createComplexRow(idx + 1, newRow)
        }
        case sortKeyType.decimalNegKeyType => {
          val bdecVal =
            try {
              Option(BigDecimal(inSource(setting.keyNr(idx))))
            } catch {
              case np: java.lang.NullPointerException => Option.empty
              case e: java.lang.NumberFormatException => Option.empty
            }
          if (bdecVal.isEmpty) {
            return Left(s"Could not read bigdecimal at col ${setting.keyNr(idx)}")
          }
          val newRow = outRow.copy(v2 = outRow.v2 :+ bdecVal.get)
          createComplexRow(idx + 1, newRow)
        }
        case sortKeyType.stringKeyType | sortKeyType.stringNegKeyType => {
          val strVal = inSource.lift(setting.keyNr(idx))
          if (strVal.isEmpty || (strVal.get == null)) {
            return Left(s"Could not read stringval at col ${setting.keyNr(idx)}")
          }
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ strVal.get)
          createComplexRow(idx + 1, newRow)
        }
        case sortKeyType.stringLowerCaseKeyType | sortKeyType.stringLowerCaseNegKeyType => {
          val strVal = inSource.lift(setting.keyNr(idx)).map(_.toLowerCase)
          if (strVal.isEmpty || (strVal.get == null)) {
            return Left(s"Could not read stringval at col ${setting.keyNr(idx)}")
          }
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ strVal.get)
          createComplexRow(idx + 1, newRow)
        }
        case _ => Left(s"Unknown sortKeyType: ${setting.keyType(idx)}")
      }
    }
    val emptyRow = if (addSource) {
      VaryRowComplex(Nil, Nil, Nil, setting.keyType, inSource)
    } else {
      VaryRowComplex(Nil, Nil, Nil, setting.keyType, Array[String]())
    }
    createComplexRow(0, emptyRow)
  }

}
