import java.io.File
import java.nio.file.{Files, Path}

import com.univocity.parsers.csv.{CsvParser, CsvParserSettings, CsvWriter, CsvWriterSettings}
import com.univocity.parsers.tsv.{TsvParser, TsvParserSettings, TsvWriter, TsvWriterSettings}

import scala.util.Random


object Tools {
  object sortKeyType extends Enumeration {
    type sortKeyType = Value
    val numericKeyType, stringKeyType = Value
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

  def writeToFile(data: List[SortableRow], fileLoc: File, settings: ExortSetting) = {
    val writer = new TsvWriter(fileLoc, new TsvWriterSettings)
    val outArray: List[Array[String]] = data.map((x: SortableRow) => x.getContent())
    outArray.foreach(writer.writeRow(_))
    writer.close()
  }

  def cleanDirectory(location: Path): Unit = {
    // Clean the directory and all files within here
  }

}
