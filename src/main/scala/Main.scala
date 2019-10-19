import java.io.File

import Tools.sortKeyType._
import com.univocity.parsers.csv.CsvWriter
import com.univocity.parsers.tsv.{TsvWriter, TsvWriterSettings}

case class ExortSetting(file: File,
                        rowSplit: Int = 20000,
                        sep: Char = ';',
                        keyType: List[sortKeyType] = stringKeyType :: Nil,
                        keyNr: List[Int] = 0 :: Nil,
                        complexSort: Boolean = false,
                        unixPrepare: Boolean = false,
                        skipHeader: Boolean = false)

object Main {

  /**
   * Simply copy the key value to the first location in the csv-file
   * (easy for use with unix sort which might have problems with keys
   * in combination with difficult quoted columns)
   */
  def unixSortPrepare(settings: ExortSetting) = {
    val outFile = new File("unix_sort_rdy.tsv")
    val writer = new TsvWriter(outFile, new TsvWriterSettings)

    val parser = settings.sep match {
      case '\t' => Tools.tsvParser(settings.skipHeader)
      case _ => Tools.csvParser(settings.sep, settings.skipHeader)
    }

    def unixReadyLine(line: Array[String]) = {
      val keyElem = line(settings.keyNr(0))
      val newLine = Array(keyElem) ++ line
      writer.writeRow(newLine: _*)
    }

    val iterCsv = parser.iterate(settings.file)
    iterCsv.forEach((x) => unixReadyLine(x))
    writer.close()
  }

  /**
   * Print the help instructions on how to use this program
   */
  def printHelp() = {
    val bighelp =
      """Sort CSV files larger than memory
done by splitting up a file and using mergesort on the parts
java -jar Exort.jar --rows 80000 --sep , myfile.csv"""
    print(bighelp)
  }

  /**
   * A method to read the commandline arguments
   *
   * @param args
   * @return
   */
  def parseArgs(args: Array[String]): Either[String, ExortSetting] = {
    val fileLoc = args(args.length - 1)
    val file = new File(fileLoc)
    if (!file.exists) {
      return Left(s"File must already exist, tried file ${file.getName}")
    }
    val defaultOption = ExortSetting(new File(fileLoc))

    // @scala.annotation.tailrec  // WORKS in 2.12.9 but doesn't in 2.13.0/1 (and intellij marks it as tail-recursive)
    def readArguments(arguments: List[String], options: ExortSetting): Either[String, ExortSetting] = {
      arguments match {
        case "--rows" :: rws :: tail => {
          val rows = try {
            rws.toInt
          } catch {
            case _: Throwable => return Left("Could not read row number")  // Dirty solution for now
          }
          if (rows < 0) {
            Left("Row value must be positive")
          } else {
            return readArguments(tail, options.copy(rowSplit = rows))
          }
        }
        case "--sep" :: sepr :: tail => {
          readArguments(tail, options.copy(sep = sepr(0)))
        }
        case "--key" :: kn :: tail => {
          val keyNumbers = try {
            kn.split(",").map(_.toInt)
          } catch {
            case _: Throwable => return Left("Could not read key number")
          }
          val invalidKeys = keyNumbers.filter(_ < 0)
          if (invalidKeys.length < 0) {
            Left("Key value must be positive")
          } else {
            return readArguments(tail, options.copy(keyNr = keyNumbers.toList))
          }
        }
        case "--keyVal" :: kv :: tail => {
          val ktypes: List[sortKeyType] = kv.split(",").toList.map((x: String) => x match {
            case "d" => decimalKeyType
            case "i" => integerKeyType
            case "s" => stringKeyType
            case _ => return Left("Could not read key type")
          })
          return readArguments(tail, options.copy(keyType = ktypes))
        }
        case "--unixsort" :: tail => {
          return readArguments(tail, options.copy(unixPrepare = true))
        }
        case "--complex" :: tail => {
          return readArguments(tail, options.copy(complexSort = true))
        }
        case Nil => Right(options)
        case _ => Left(s"Unknown options ${arguments}")
      }
    }

    /**
     * Make sure that the amount of keytypes equals the amount of keylocations
     */
    def fixSettings(settings: ExortSetting): ExortSetting = {
      if (settings.keyNr.length < settings.keyType.length) {
        settings.copy(keyType = settings.keyType.slice(0, settings.keyNr.length))
      } else if (settings.keyType.length > settings.keyType.length) {
        val diff = settings.keyType.length - settings.keyType.length
        settings.copy(keyType = settings.keyType ++ List.fill(diff)(stringKeyType))
      } else {
        settings
      }
    }

    if (args.length > 1) {
      val otherArguments = args.slice(0, args.length - 1)
      val options = readArguments(otherArguments.toList, defaultOption)
      return options.map(fixSettings(_))
    }
    Right(defaultOption)
  }

  /**
   * Program entry point
   *
   * @param args command line arguments
   */
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      printHelp
      return
    }
    val settings = parseArgs(args)
    settings match {
      case Right(rsettings) => {
        if (rsettings.unixPrepare) {
          unixSortPrepare(rsettings)
        } else {
          ExtSort.runExternalSort(rsettings)
        }
      }
      case Left(errMsg) => println(errMsg)
    }
  }
}
