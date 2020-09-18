package fold.parse.idris.para

object ParaGrammar {

  import fastparse.NoWhitespace._
  import fastparse._

  import ParaLexical._

  case class Identifier(name: String)

  def somethingTo[_: P]: P[Identifier] = P(
    identifier ~ space ~ "to" ~ space ~ identifier ~ P(space ~ "to" ~ space ~ identifier).rep(0)).map((f) => {
      Identifier("")
    })

  def sentence[_: P] = P(
    // something is something to something to something
    identifier ~ space ~ "is" ~ space ~ somethingTo ~ End
  ).map((f) => Identifier("") )

  def optSpace[_: P] = CharIn(" ").rep(0)
  def space[_: P] = CharIn(" ").rep(1)


}

