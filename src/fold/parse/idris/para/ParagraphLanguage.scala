package fold.parse.idris.para

object ParagraphLanguage extends App {

  import fastparse._

  def sentenceToIdris(sentence: String) = {
    val all = sentence.split("\n")
    for (a <- all) {
      val result = parse(a, ParaGrammar.sentence(_))
      println(result)
      println
    }
  }

  def test() = {
    val what =
      """

        |
        |""".stripMargin

    val sentence = """average is string to double
                    |average of str is cast total length / cast num words
                    |total length is sum of all lengths of words str
                    |num words is word count of str
                    |word count is string to nat
                    |word count of str is the length of words of str
                    |all lengths of strs is map lengths strs
                    |show average is string to string
                    |show average of str is 'The average word length is:'""".stripMargin

    val idris = sentenceToIdris(sentence)

    println(idris)
  }
  test()

/*  def testAverageDef = {

    val str = "average is string to double"
    // average : String -> Double
    val result = parse(str, ParaGrammar.sentence(_))
    println(result)
  }
  testAverageDef

  def testAverageImpl = {
    val str = "average of str is cast total length / cast num words"
    // average = cast totalLength / cast numWords
    val result = parse(str, ParaGrammar.sentence(_))
    println(result)
  }
  testAverageImpl

  def testTotalLength = {
    //
    val str = "total length is sum of all lengths of words str"
    val result = parse(str, ParaGrammar.sentence(_))
    println(result)
  }
  testTotalLength

  def testNumWords = {
    //
    val str = "num words is word count of str"
    val result = parse(str, ParaGrammar.sentence(_))
    println(result)
  }
  testNumWords*/

}

