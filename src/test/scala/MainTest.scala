import org.scalatest._

class MainTest extends FlatSpec with Matchers {
  "no extra argument options" should "give default settings" in {
    val testFileLoc = "testbig.csv"
    val simpleInvocation = Array(testFileLoc)
    val options = Main.parseArgs(simpleInvocation).toOption.get
    options.file.getName === testFileLoc
  }

  "single extra argument to options" should "change the default option" in {
    val testFileLoc = "testbig.csv"
    val invocation1 = Array("--rows", "42", testFileLoc)
    val options1 = Main.parseArgs(invocation1).toOption.get
    options1.file.getName == testFileLoc
    options1.rowSplit === 42

    val invocation2 = Array("--sep", "|", testFileLoc)
    val options2 = Main.parseArgs(invocation2).toOption.get
    options2.file.getName === testFileLoc
    options2.sep === "|"
  }

  "multiple extra options " should "change all the options" in {
    val testFileLoc = "testbig.csv"
    val invocation = Array("--rows", "64", "--sep", "&", testFileLoc)
    val options = Main.parseArgs(invocation).toOption.get
    options.file.getName === testFileLoc
    options.sep === "&"
    options.rowSplit === 64
  }

}
