package dev.hillman.exort

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MainTest extends AnyFlatSpec with Matchers {
  "A single argument invocation" should "give default settings" in {
    val testFileLoc = "src/test/resources/testfileA.csv"
    val simpleInvocation = Array(testFileLoc)
    val options = Main.parseArgs(simpleInvocation).toOption.get
    options.file.getName === testFileLoc
  }

  "adding extra parameters" should "change the default option" in {
    val testFileLoc = "src/test/resources/testfileA.csv"
    val fileName = "testfileA.csv"
    val invocation1 = Array("--rows", "42", testFileLoc)
    val options1 = Main.parseArgs(invocation1).toOption.get
    assert(options1.file.getName === fileName)
    assert(options1.rowSplit === 42)

    val invocation2 = Array("--sep", "|", testFileLoc)
    val options2 = Main.parseArgs(invocation2).toOption.get
    assert(options2.file.getName === fileName)
    assert(options2.sep === '|')

    val invocation3 = Array("--rows", "42M", testFileLoc)
    val options3 = Main.parseArgs(invocation3).toOption.get
    assert(options3.file.getName == fileName)
    assert(options3.rowSplit === 42000000)

    val invocation4 = Array("--out", "totallydifferent.tsv", testFileLoc)
    val options4 = Main.parseArgs(invocation4).toOption.get
    assert(options4.file.getName == fileName)
    assert(options4.outFileName === "totallydifferent.tsv")

    val invocation5 = Array("--rows", "64", "--sep", "&", testFileLoc)
    val options5 = Main.parseArgs(invocation5).toOption.get
    assert(options5.file.getName === fileName)
    assert(options5.sep === '&')
    assert(options5.rowSplit === 64)

    val invocation6 = Array("--key", "6,2", "--keyVal", "-d,s", testFileLoc)
    val options6 = Main.parseArgs(invocation6).toOption.get
    assert(options6.keyType === List(Tools.sortKeyType.decimalNegKeyType, Tools.sortKeyType.stringKeyType))
    assert(options6.keyNr === List(6, 2))
  }

  "setting the wrong keyValue param" should "result in an error" in {
    val testFileLoc = "src/test/resources/testfileA.csv"
    val invoc1 = Array("--key", "4,2", "--keyVal", "-b,s", testFileLoc)
    val opt1 = Main.parseArgs(invoc1)
    assert(opt1.isLeft)
  }

}
