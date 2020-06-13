package fold.parse.idris

import fastparse.NoWhitespace._
import fastparse.P

object Grammar {
  import fastparse._

  /*trait Identifier {
    def name: String
  }*/
  case class Identifier(name: String)
  case class ArrayIdentifier(name: String)
  case class MethodCall(method: Identifier, parameter: Seq[Product])
  case class MethodDefinition(name: String, rest: Seq[MethodDefinitionParameter])
  case class MethodDefinitionParameter(firstParam: Identifier, rest: Seq[Identifier])
  case class MethodNameBindings(methodName: Identifier, rest: Seq[Identifier])
  case class MethodLine(left: MethodNameBindings, methodCall: MethodCall, methodImplWhere: MethodImplWhere)
  case class Method(methodDefinition: MethodDefinition, methodLine: MethodLine)
  case class MethodImplWhere(methodDefinition: MethodDefinition, patterns: Seq[PatternMatch])
  case class PatternMatch(first: Identifier, rest: Seq[Product], methodCall: MethodCall)

  def method[_: P] = P ( optSpace ~ methodDecl ~ optSpace ~ newLine ~ optSpace ~ methodImpl ~ optSpace ~ newLine ~ End)
    .map(f => Method(f._1, f._2))

  def newLine[_: P] = P("\n")

  def optionalParameter[_: P] = P ( optSpace ~ "->" ~ optSpace ~ methodDeclParameter )

  // Method definition
  def methodDecl[_: P] = P ( identifier_t ~ optSpace ~ ":" ~ optSpace ~ methodDeclParameters)
    .map(f => MethodDefinition(f._1.name,
      f._2._1 +: f._2._2))
  def methodDeclParameters[_ : P] = P ( methodDeclParameter ~ optionalParameter.rep )
  def methodDeclParameter[_: P] = P(option1) // | option2 | option3)
  def option1[_: P] = P(identifier_t ~ P(space ~ identifier_t).rep(0)).map(
    (f: (Identifier, Seq[Identifier])) => MethodDefinitionParameter(f._1, f._2)
  )
  def option2[_: P] = P("(" ~ optSpace ~ Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0) ~ optSpace ~ ")")
  def option3[_: P] = P("{" ~ optSpace ~ Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0) ~ optSpace ~ "}")

  // Method implementation
  // eg.
  //  reverse xs = revAcc [] xs where
  //    revAcc : List a -> List a -> List a
  //    revAcc acc [] = acc
  //    revAcc acc (x :: xs) = revAcc (x :: acc) xs
  def methodImpl[_: P] = P ( identifiers.map(f => MethodNameBindings(f._1, f._2)) ~ optSpace ~ "=" ~ optSpace ~ methodCall ~ space ~
    methodImplWhere )
    .map(f => MethodLine(f._1, f._2, f._3))

  def methodImplWhere[_: P] = P( &("where") ~ "where" ~ optSpace ~ newLine ~ optSpace ~ methodDecl ~ optSpace ~ newLine ~ multiplePatternMatches).map(f => {
    MethodImplWhere(f._1, f._2)
  })

  def multiplePatternMatches[_: P] = P(optSpace ~ patternMatch ~ optSpace ~ newLine).rep(1)

  def myMethodImpl[_: P] = methodImpl

  // Calling a method
  def methodCall[_: P] = P ( methodCallIdentifiers ).map(f => MethodCall(f._1, {
    for (n <- f._2.toList) yield {
      n match {
        case Identifier(s) => Identifier(s)
        case ArrayIdentifier(s) => Identifier(s)
        case _ => n
      }
    }
  }))
  def methodCallIdentifiers[_: P] = P(methodCallIdentifier) ~ P(!P(space ~ "where") ~ space ~ methodCallParameter).rep(0)
  //def methodCallIdentifiers2[_: P] = P(methodCallIdentifier) ~ P(space ~ methodCallParameter).rep(0)
  def methodCallIdentifier[_: P] = P(Lexical.identifier)
  def methodCallParameter[_: P] = P(!P("where") ~ P(emptyArray | bracketedMethodCallParameter | Lexical.identifier))

  def bracketedMethodCallParameter[_: P] = P("(" ~ optSpace ~ Lexical.identifier ~ optSpace ~ "::" ~ optSpace ~ Lexical.identifier ~ optSpace ~ ")")

  def patternMatch[_: P] = P(Lexical.identifier ~ patternMatchIdentifiers ~ optSpace ~ "=" ~ optSpace ~ methodCall).map(f => {
    PatternMatch(f._1, f._2, f._3)
  })
  def patternMatchIdentifiers[_: P] = patternMatchIdentifier.rep
  def patternMatchIdentifier[_: P] = P(space ~ !("=") ~ P(Lexical.identifier | arrayPatternMatch | listPatternMatch))

  def arrayPatternMatch[_: P] = emptyArray
  def listPatternMatch[_: P] = P("(" ~ optSpace ~ Lexical.identifier ~ optSpace ~ "::" ~ optSpace ~ Lexical.identifier ~ ")")

  def identifiers[_: P] = P(Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0))




  def identifier_t[_: P] = Lexical.identifier

  def optSpace[_: P] = CharIn(" ").rep(0)
  def space[_: P] = CharIn(" ").rep(1)

  def moduleHeader[_: P] = P( "module" ~ space ~ identifier_t ~ optSpace ~ ";" )

  //-----------------------------

  def importLine[_: P] = P ( "import" ~ space ~ identifier_t ~ optSpace ~ ";" )


  // Matching an empty array such as [] or [ ]
  def emptyArray[_: P] = P ( "[" ~ optSpace ~ "]" ).map(f => ArrayIdentifier("[]"))

  // Documentation

  def SingleLineComment_t[_: P] = P( "--" ~ CharsWhile(_ != '\n', 0) )
  def DocCommentLine[_: P] = P ( "|||" ~ CharsWhile(_ != '\n', 0) )
  def ArgCommentLine[_: P] = P ( "|||" ~ optSpace ~ "@" ~ CharsWhile(_ != '\n', 0) )
}
