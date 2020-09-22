package fold.parse.idris.para

object ParaGrammar {

  import fastparse.NoWhitespace._
  import fastparse._

  import ParaLexical._

  case class Identifier(name: String)

  def quotedString[_: P] = {
    shortstring
  }

  def sentenceTo[_: P]: P[Identifier] = P(
    &(P(identifier ~ space).rep(1) ~ "to") ~
      P(identifier ~ space).rep(1) ~ "to" ~ space ~ identifier ~ P(space ~ "to" ~ space ~ identifier).rep(0)).map((f) => {
    Identifier("")
  })

  def ofAndIdentifier[_: P] = P(
    identifier | "of"
  )

  def sentenceTo2[_: P]: P[Identifier] = P(
      ofAndIdentifier ~
      P(space ~ ofAndIdentifier).rep(0)).map((f) => {
    Identifier("")
  })


  def sentenceIs[_: P] = P(
    // something is something to something to something
    &(P(identifier ~ space).rep(1) ~ "is" ) ~
    P(identifier ~ space).rep(1) ~ "is" ~ space ~ sentenceTo2 ~ End
  )

  def expression[_: P] = {
    P("/")
  }

  def some[_: P] = P(
    expression |
      identifier | "of" |
      "the" | quotedString
  )

  // P("cast total length / cast num words")
  // the length of words of str
  def sentenceImpl[_: P]: P[Identifier] = {
    P(some ~ P(space ~ some).rep(0)).map((f) => {
      Identifier("")
    })
  }

  def sentenceOf[_: P] = P(
    // word count of str is the length of words of str
    &(P(identifier ~ space).rep(1) ~ "of") ~
      P(P(identifier ~ space).rep(1) ~ "of" ~ space ~ P(identifier ~ space).rep(1) ~ P(&("is") ~ "is" ~ space ~ sentenceImpl).rep(0) ~ End)
  )

  def sentence[_: P] = P(
    // something is something to something to something
    sentenceIs | sentenceTo | sentenceOf
  ).map((f) => Identifier("") )

  // average of str is cast total length / cast num words

  def optSpace[_: P] = CharIn(" ").rep(0)
  def space[_: P] = CharIn(" ").rep(1)


}

