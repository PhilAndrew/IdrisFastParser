package fold.parse.idris

import fastparse.NoWhitespace._
import fastparse.P

object Grammar {
  import fastparse._

  def method[_: P] = P ( methodDecl ~ optSpace ~ newLine ~ methodImpl ~ optSpace ~ newLine)

  def newLine[_: P] = P("\n")

  def methodParameters[_ : P] = P ( methodParameter ~ optionalParameter.rep )
  def optionalParameter[_: P] = P ( optSpace ~ "->" ~ optSpace ~ methodParameter )
  // Method definition
  def methodDecl[_: P] = P ( identifier_t ~ optSpace ~ ":" ~ optSpace ~ methodParameters)
  // Method implementation
  // eg. reverse xs = revAcc [] xs where
  def methodImpl[_: P] = P ( identifiers ~ space ~ "=" ~ methodCall ~ P(space ~ "where") )

  // Calling a method
  def methodCall[_: P] = P ( identifiersInMethodCall )
  def identifiersInMethodCall[_: P] = P(identifierInMethodCall) ~ P(space ~ identifierInMethodCall).rep(0)
  def identifierInMethodCall[_: P] = P(emptyArray | Lexical.identifier)

  def methodParameter[_: P] = P(identifiers | option2 | option3)

  def identifiers[_: P] = P(Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0))
  def option2[_: P] = P("(" ~ optSpace ~ Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0) ~ optSpace ~ ")")
  def option3[_: P] = P("{" ~ optSpace ~ Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0) ~ optSpace ~ "}")




  def identifier_t[_: P] = Lexical.identifier

  def optSpace[_: P] = CharIn(" ").rep(0)
  def space[_: P] = CharIn(" ").rep(1)

  def moduleHeader[_: P] = P( "module" ~ space ~ identifier_t ~ optSpace ~ ";" )

  //-----------------------------

  def importLine[_: P] = P ( "import" ~ space ~ identifier_t ~ optSpace ~ ";" )


  // Matching an empty array such as [] or [ ]
  def emptyArray[_: P] = P ( "[" ~ optSpace ~ "]" ).!

  // Documentation

  def SingleLineComment_t[_: P] = P( "--" ~ CharsWhile(_ != '\n', 0) )
  def DocCommentLine[_: P] = P ( "|||" ~ CharsWhile(_ != '\n', 0) )
  def ArgCommentLine[_: P] = P ( "|||" ~ optSpace ~ "@" ~ CharsWhile(_ != '\n', 0) )
}
