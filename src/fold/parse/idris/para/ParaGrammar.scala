package fold.parse.idris.para

object ParaGrammar {

  import fastparse.NoWhitespace._
  import fastparse._

  import ParaLexical._

  case class Identifier(name: String)

  def sentenceTo[_: P]: P[Identifier] = P(
    &(identifier ~ space ~ "to") ~
    identifier ~ space ~ "to" ~ space ~ identifier ~ P(space ~ "to" ~ space ~ identifier).rep(0)).map((f) => {
      Identifier("")
    })

  def sentenceIs[_: P] = P(
    // something is something to something to something
    identifier ~ space ~ "is" ~ space ~ sentenceTo ~ End
  )

  def sentenceImpl[_: P] = {
    P("cast total length / cast num words")
  }

  def sentenceOf[_: P] = P(
    // something is something to something to something
    &(identifier ~ space ~ "of") ~
    identifier ~ space ~ "of" ~ space ~ identifier ~ space ~ "is" ~ space ~ sentenceImpl ~ End
  )

  def sentence[_: P] = P(
    // something is something to something to something
    sentenceIs | sentenceOf
  ).map((f) => Identifier("") )

  // average of str is cast total length / cast num words

  def optSpace[_: P] = CharIn(" ").rep(0)
  def space[_: P] = CharIn(" ").rep(1)


}

