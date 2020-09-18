package fold.parse.idris.para

object ParagraphLanguage extends App {

  import fastparse._

  def sentenceToIdris(sentence: String) = {
    ???
  }

  def test() = {
    val sentence = """average is string to double
      |average of str is cast total length / cast num words
      |total length is sum of all lengths of words str
      |num words is word count of str
      |word count is string to nat
      |word count of str is the length of words of str
      |all lengths of strs is map lengths strs
      |show average is string to string
      |show average of str is "The average word length is:" ++
      |show the average of str ++ "\n"""".stripMargin

    val idris = sentenceToIdris(sentence)

    println(idris)
  }

  def testAverageDef = {
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
  //testAverageImpl

}

