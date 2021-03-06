package dev.hillman.exort

import dev.hillman.exort.Main.addExtraFile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class MainTest extends AnyFlatSpec with Matchers {
  "A single argument invocation" should "give default settings" in {
    val testFileLoc = "src/test/resources/testfileA.csv"
    val simpleInvocation = Array(testFileLoc)
    val options = Main.parseArgs(simpleInvocation).toOption.get
    options.files.head.getName === testFileLoc
  }

  "adding extra parameters" should "change the default option" in {
    val testFileLoc = "src/test/resources/testfileA.csv"
    val fileName = "testfileA.csv"
    val invocation1 = Array("--rows", "42", testFileLoc)
    val options1 = Main.parseArgs(invocation1).toOption.get
    assert(options1.files.head.getName === fileName)
    assert(options1.rowSplit === 42)

    val invocation2 = Array("--sep", "|", testFileLoc)
    val options2 = Main.parseArgs(invocation2).toOption.get
    assert(options2.files.head.getName === fileName)
    assert(options2.sep === '|')

    val invocation3 = Array("--rows", "42M", testFileLoc)
    val options3 = Main.parseArgs(invocation3).toOption.get
    assert(options3.files.head.getName == fileName)
    assert(options3.rowSplit === 42000000)

    val invocation4 = Array("--out", "totallydifferent.tsv", testFileLoc)
    val options4 = Main.parseArgs(invocation4).toOption.get
    assert(options4.files.head.getName == fileName)
    assert(options4.outFileName === "totallydifferent.tsv")

    val invocation5 = Array("--rows", "64", "--sep", "&", testFileLoc)
    val options5 = Main.parseArgs(invocation5).toOption.get
    assert(options5.files.head.getName === fileName)
    assert(options5.sep === '&')
    assert(options5.rowSplit === 64)

    val invocation6 = Array("--key", "6,2", "--keyVal", "-d,s", testFileLoc)
    val options6 = Main.parseArgs(invocation6).toOption.get
    assert(
      options6.keyType === List(Tools.sortKeyType.decimalNegKeyType,
                                Tools.sortKeyType.stringKeyType))
    assert(options6.keyNr === List(6, 2))
  }

  "setting the wrong keyValue param" should "result in an error" in {
    val testFileLoc = "src/test/resources/testfileA.csv"
    val invoc1 = Array("--key", "4,2", "--keyVal", "-b,s", testFileLoc)
    val opt1 = Main.parseArgs(invoc1)
    assert(opt1.isLeft)
  }

  "file insertion when reading file parameters" should "be added infront of the last file position" in {
    val lastFile = List(new File("d"))
    val otherFiles = List("a", "b", "c").map(l => new File(l))
    val out1 = addExtraFile(otherFiles(0), lastFile)
    assert(out1 === List("a", "d").map(l => new File(l)))
    val out2 = addExtraFile(otherFiles(1), out1)
    assert(out2 === List("a", "b", "d").map(l => new File(l)))
    val out3 = addExtraFile(otherFiles(2), out2)
    assert(out3 === List("a", "b", "c", "d").map(l => new File(l)))
  }

  "adding multiple files" should "result in a list of file parameters loaded" in {
    val testFileLoc1 = "src/test/resources/testfileA.csv"
    val testFileLoc2 = "src/test/resources/testfileAA.csv"
    val testFileLoc3 = "src/test/resources/testfileVary.csv"
    val invoc = Array("--key",
                      "4,2",
                      "--keyVal",
                      "-d,s",
                      testFileLoc1,
                      testFileLoc2,
                      testFileLoc3)
    val opt = Main.parseArgs(invoc)
    assert(opt.isRight)
    opt.foreach(
      s =>
        assert(s.files
          .map(_.toString) === List(testFileLoc1, testFileLoc2, testFileLoc3)))
  }

}
