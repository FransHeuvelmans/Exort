import java.io.File
import java.nio.file.{Files, Path}

import com.univocity.parsers.csv.{CsvParser, CsvParserSettings, CsvWriter, CsvWriterSettings}
import com.univocity.parsers.tsv.{TsvParser, TsvParserSettings, TsvWriter, TsvWriterSettings}

import scala.util.Random


object Tools {
  object sortKeyType extends Enumeration {
    type sortKeyType = Value
    val decimalKeyType, integerKeyType, stringKeyType = Value
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
    val tempPrefix = "exsort_" + Random.alphanumeric.take(10).foldLeft("")((x: String, y: Char) => x + y)
    Files.createTempDirectory(tempPrefix)
  }

  def tempOutputFile(location: Path, nr: Int = 0): File = new File(location.toFile, s"p${nr}.tsv")

  def writeToFile(data: Iterable[SortableRow], fileLoc: File, settings: ExortSetting) = {
    val writer = new TsvWriter(fileLoc, new TsvWriterSettings)
    val outArray: Iterable[Array[String]] = data.map((x: SortableRow) => x.getContent)
    outArray.foreach(writer.writeRow(_))
    writer.close()
  }

  /**
   * Alphabetical distance (NO edit distance)
   */
  @scala.annotation.tailrec
  def StringDistance(a: String, b: String): Double = {
    if ((a == Nil) && (b == Nil)) {
      return 0.0
    } else if (a == Nil) {
      return b.head.toDouble
    } else if (b == Nil) {
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
  }

}
