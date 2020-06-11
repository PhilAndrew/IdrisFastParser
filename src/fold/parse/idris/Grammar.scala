package fold.parse.idris

import fastparse.NoWhitespace._
import fastparse.P

object Grammar {
  import fastparse._

  def methodParameters[_ : P] = P ( aFewIdentifiers ~ optionalParameter.rep )
  def optionalParameter[_: P] = P ( maybeSpace ~ "->" ~ maybeSpace ~ aFewIdentifiers )
  def methodDecl[_: P] = P ( identifier_t ~ maybeSpace ~ ":" ~ maybeSpace ~ methodParameters)

  def aFewIdentifiers[_: P] = P(option1 | option2 | option3)

  def option1[_: P] = P(Lexical.identifier ~ P(someSpace ~ Lexical.identifier).rep(0))
  def option2[_: P] = P("(" ~ maybeSpace ~ Lexical.identifier ~ P(someSpace ~ Lexical.identifier).rep(0) ~ maybeSpace ~ ")")
  def option3[_: P] = P("{" ~ maybeSpace ~ Lexical.identifier ~ P(someSpace ~ Lexical.identifier).rep(0) ~ maybeSpace ~ "}")

  def docComment_t[_: P] = P ( "|||" )
  def identifier_t[_: P] = Lexical.identifier

  def maybeSpace[_: P] = CharIn(" ").rep(0)
  def someSpace[_: P] = CharIn(" ").rep(1)

  def moduleHeader[_: P] = P( docComment_t.? ~ "module" ~ someSpace ~ identifier_t.? ~ maybeSpace ~ ";" )

}
