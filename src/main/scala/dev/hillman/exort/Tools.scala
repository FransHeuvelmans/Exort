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

  def csvParser(sep: Char, hasHeader: Boolean): CsvParser = {
    val settings = new CsvParserSettings()
    val fmt = settings.getFormat()
    fmt.setDelimiter(sep)
    settings.getFormat.setLineSeparator("\n")
    settings.setHeaderExtractionEnabled(hasHeader)
    new CsvParser(settings)
  }

  def tsvParser(hasHeader: Boolean): TsvParser = {
    val settings = new TsvParserSettings()
    settings.getFormat.setLineSeparator("\n")
    settings.setHeaderExtractionEnabled(hasHeader)
    new TsvParser(settings)
  }

  def createTempDir(): Path = {
    val tempPrefix = "exsort_" + Random.alphanumeric
      .take(10)
      .foldLeft("")((x: String, y: Char) => x + y)
    Files.createTempDirectory(tempPrefix)
  }

  def tempOutputFile(location: Path, nr: Int = 0): File =
    new File(location.toFile, s"p${nr}.tsv")

  def writeToFile(data: Iterable[SortableRow],
                  fileLoc: File,
                  settings: ExortSetting) = {
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
  def StringDistance(a: String, b: String): Double = {
    if ((a.isEmpty) && (b.isEmpty)) {
      return 0.0
    } else if (a.isEmpty) {
      return b.head.toDouble
    } else if (b.isEmpty) {
      return -a.head.toDouble
    }
    val diff = b.head - a.head
    if (diff == 0) {
      StringDistance(a.tail, b.tail)
    } else {
      diff.toDouble
    }
  }

  def cleanDirectory(location: Path): Unit = {
    // Clean the directory and all files within here
    println(s"Deleting files at $location")
    val filePathList = Files.list(location).toScala(Seq)
    println(filePathList)
    filePathList.map((x: Path) => x.toFile.delete())
  }

  def convertToVaryRow(inSource: Array[String],
                       setting: ExortSetting,
                       addSource: Boolean = true): VaryRow = {

    @scala.annotation.tailrec
    def createRow(tempSetting: ExortSetting, outRow: VaryRow): VaryRow = {
      tempSetting.keyType match {
        case sortKeyType.integerKeyType :: tail => {
          val newRow = outRow.copy(
            v3 = outRow.v3 :+ inSource(tempSetting.keyNr.head).toInt)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.integerNegKeyType :: tail => {
          val newRow = outRow.copy(
            v3 = outRow.v3 :+ inSource(tempSetting.keyNr.head).toInt)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.decimalKeyType :: tail => {
          val newRow = outRow.copy(
            v2 = outRow.v2 :+ inSource(tempSetting.keyNr.head).toDouble)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.decimalNegKeyType :: tail => {
          val newRow = outRow.copy(
            v2 = outRow.v2 :+ inSource(tempSetting.keyNr.head).toDouble)
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.stringKeyType :: tail => {
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ inSource(tempSetting.keyNr.head))
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case sortKeyType.stringNegKeyType :: tail => {
          val newRow =
            outRow.copy(v1 = outRow.v1 :+ inSource(tempSetting.keyNr.head))
          createRow(tempSetting.copy(keyType = tempSetting.keyType.tail,
                                     keyNr = tempSetting.keyNr.tail),
                    newRow)
        }
        case Nil => outRow
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

  // Todo -> create a VaryTypeFile from a list of VeryTypeRows...
}
