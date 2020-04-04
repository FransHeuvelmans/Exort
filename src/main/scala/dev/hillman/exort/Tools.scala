package dev.hillman.exort

import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

import com.univocity.parsers.csv.{CsvParser, CsvParserSettings}
import com.univocity.parsers.tsv.{
  TsvParser,
  TsvParserSettings,
  TsvWriter,
  TsvWriterSettings
}
import Tools.sortKeyType

import scala.util.Random

object Tools {
  object sortKeyType extends Enumeration {
    type sortKeyType = Value
    val decimalKeyType, decimalNegKeyType, integerKeyType, integerNegKeyType,
    stringKeyType, stringNegKeyType = Value
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
  def writeToFile(data: Iterable[SortableRow],
                  fileLoc: File,
                  settings: ExortSetting): Unit = {
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
  private def StringDistance(a: String,
                     b: String,
                     prevDistances: List[Int]): List[Int] = {
    if ((a.isEmpty) && (b.isEmpty)) {
      return prevDistances.reverse
    } else if (a.isEmpty) {
      return (-b.head.toInt :: prevDistances).reverse
    } else if (b.isEmpty) {
      return (a.head.toInt :: prevDistances).reverse
    }
    val diff = a.head - b.head
    StringDistance(a.tail, b.tail, diff :: prevDistances)
  }

  /**
   * Alphabetical distance with case equality
   */
  def StringDistanceLowerCase(a: String, b: String): List[Int] =
    StringDistance(a.toLowerCase, b.toLowerCase, Nil)

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
  def convertToVaryRow(inSource: Array[String],
                       setting: ExortSetting,
                       addSource: Boolean = true): Either[String, VaryRow] = {

    val inSourceSafe = inSource.lift
    def getSafe(idx: Int): Option[String] = inSourceSafe(idx).flatMap(s => s match {
        case null => Option.empty
        case anS: String => Option(anS)
      })

    @scala.annotation.tailrec
    def createRow(tempSetting: ExortSetting, outRow: VaryRow): Either[String, VaryRow] = {
      tempSetting.keyType match {
        case sortKeyType.integerKeyType :: tail => {
          val longVal = getSafe(tempSetting.keyNr.head).flatMap(_.toLongOption)
          if (longVal.isEmpty) {
            return Left(s"Could not read longval at col ${tempSetting.keyNr.head}")
          }
          val newRow = outRow.copy(
            v3 = outRow.v3 :+ longVal.get)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.integerNegKeyType :: tail => {
          val longVal = getSafe(tempSetting.keyNr.head).flatMap(_.toLongOption)
          if (longVal.isEmpty) {
            return Left(s"Could not read longval at col ${tempSetting.keyNr.head}")
          }
          val newRow = outRow.copy(
            v3 = outRow.v3 :+ longVal.get)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.decimalKeyType :: tail => {
          val doubleVal = getSafe(tempSetting.keyNr.head).flatMap(_.toDoubleOption)
          if (doubleVal.isEmpty) {
            return Left(s"Could not read doubleval at col ${tempSetting.keyNr.head}")
          }
          val newRow = outRow.copy(
            v2 = outRow.v2 :+ doubleVal.get)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.decimalNegKeyType :: tail => {
          val doubleVal = getSafe(tempSetting.keyNr.head).flatMap(_.toDoubleOption)
          if (doubleVal.isEmpty) {
            return Left(s"Could not read doubleval at col ${tempSetting.keyNr.head}")
          }
          val newRow = outRow.copy(
            v2 = outRow.v2 :+ doubleVal.get)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.stringKeyType :: tail => {
          val strVal = getSafe(tempSetting.keyNr.head)
          if (strVal.isEmpty) {
            return Left(s"Could not read stringval at col ${tempSetting.keyNr.head}")
          }
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ strVal.get)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.stringNegKeyType :: tail => {
          val strVal = getSafe(tempSetting.keyNr.head)
          if (strVal.isEmpty) {
            return Left(s"Could not read stringval at col ${tempSetting.keyNr.head}")
          }
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ strVal.get)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case Nil => Right(outRow)
        case _ =>
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    outRow)
      }
    }
    val emptyRow = if (addSource) {
      VaryRow(Nil, Nil, Nil, setting.keyType, inSource)
    } else {
      VaryRow(Nil, Nil, Nil, setting.keyType, Array[String]())
    }
    createRow(setting, emptyRow)
  }

}
