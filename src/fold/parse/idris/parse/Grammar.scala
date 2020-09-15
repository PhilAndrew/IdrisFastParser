package fold.parse.idris.parse

import fold.parse.idris.parse.Lexical

object Grammar {

  import fastparse.NoWhitespace._
  import fastparse._

  /*trait Identifier {
    def name: String
  }*/
  case class Identifier(name: String)
  case class ArrayIdentifier(name: String, isEmpty: Boolean)
  case class MethodDefinition(name: String, parameters: Seq[MethodDefinitionParameter])
  case class MethodDefinitionParameter(param: Identifier, rest: Seq[Identifier])

  case class MethodParameter(name: Option[String], bracketedForm: Option[Bracketed] = None, extractionForm: Option[MethodExpression] = None)
  case class MethodNameBindings(methodName: Identifier, rest: Seq[MethodParameter])

  case class Method(methodDefinition: MethodDefinition, patternMatch: Seq[MethodLine])
  case class MethodLine(left: MethodNameBindings, statement: DataValueOrMethodCall, methodImplWhere: Option[Method])

  //case class SuccessorMethodCall(dataValue: DataValueOrMethodCall)
  case class DataValueOrMethodCall(dataValue: Option[DataValue], methodCall: Option[MethodCall])
  case class MethodCall(method: Identifier, parameter: Seq[Product], isReferenceNotMethodCall: Boolean = false, isDataResultNotMethodCall: Boolean = false, isSuccessorMethodCall: Boolean = false)
  case class DataValue(name: Identifier, rest: Seq[Identifier])

  trait ExpressionType
  case class HeadTailExpression() extends ExpressionType
  case class ListAppendExpression() extends ExpressionType
  case class MethodExpression(expressionType: ExpressionType, first: Identifier, second: Identifier)

  case class Bracketed(first: Identifier, second: Seq[Identifier])

  case class DataType(first: Identifier, second: Seq[Identifier])
  case class DataConstructor()
  case class Data(dataType: DataType, constructors: Seq[DataConstructor])

  case class ParsedFile(method: Option[Method])

  def consumeEmptyLines[_: P] = P(optSpace ~ newLine).rep(0)

  def fileContents[_: P] = P(P(consumeEmptyLines ~ optSpace ~ block ~ optSpace).rep(0)  ~ consumeEmptyLines ~ End).map((f) => {
    ParsedFile(None)
  })

  def block[_: P] = P(dataDefinition | method)

  // data Pair a b = MkPair a b
  def dataDefinition[_: P] = P("data" ~ space ~ "Pair" ~ space ~ "a" ~ space ~ "b" ~ space ~ "=" ~ space ~ "MkPair" ~ space ~ "a" ~ space ~ "b").map((d) => {
    Data(DataType(Identifier("Pair"), Seq.empty), Seq.empty)
  })

  def method[_: P] = P ( optSpace ~ methodDecl ~ optSpace ~
                                      P(newLine ~ optSpace ~ methodImpl ~ optSpace).rep(0) ~ consumeEmptyLines ~ End)
    .map(f => ParsedFile(Some(Method(f._1, f._2))))

  def newLine[_: P] = P("\n")

  // Method definition
  def methodDecl[_: P] = P ( identifier_t ~ optSpace ~ ":" ~ optSpace ~ methodDeclParameters)
    .map(f => MethodDefinition(f._1.name,
      f._2._1 +: f._2._2))
  def optionalParameter[_: P] = P ( optSpace ~ "->" ~ optSpace ~ methodDeclParameter )
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
  def methodImpl[_: P]: P[MethodLine] = P ( methodImplLeft.map((f) => {

    /*def allAreIdentifiers(i: Seq[Product]): Boolean = i.forall(_.isInstanceOf[Identifier])
    def it(product: Product) = {
      product match {
        case i: Identifier => ???
        case b: Bracketed => ???
      }
    }*/



    if ((f._1.isInstanceOf[Identifier]) && (f._2.forall(_.isInstanceOf[Identifier]))) {
      MethodNameBindings(f._1.asInstanceOf[Identifier], f._2.asInstanceOf[Seq[Identifier]].map(m => MethodParameter(Some(m.name))))
    } else {
      // @todo Wrong, incomplete
      if (f._2.forall(_.isInstanceOf[Bracketed])) {
        val b = f._2.asInstanceOf[Seq[Bracketed]]
        MethodNameBindings(f._1.asInstanceOf[Identifier], b.map((m: Bracketed) => MethodParameter(None,
          bracketedForm = Some(Bracketed(Identifier("S"), Seq(Identifier("k")))))))
      } else {
        val params: Seq[MethodParameter] = for (s: Product <- f._2) yield {
          s match {
            case i: Identifier => MethodParameter(Some(i.name))
            case a: ArrayIdentifier => MethodParameter(Some(a.name))
            case e: MethodExpression => MethodParameter(None, extractionForm = Some(e))
            case _ => {
              MethodParameter(None)
            }
          }
        }
        // 0 Identifier, 1 ArrayIdentifier
        MethodNameBindings(f._1.asInstanceOf[Identifier], params)
      }
    }
  }) ~ optSpace ~ "=" ~ optSpace ~ methodImplRightSide ~
    P(&(space ~ "where") ~ space ~ methodImplWhere).? )
    .map(f => {
      MethodLine(f._1, f._2, f._3)
    })

  def methodImplLeft[_: P] = P(methodImplLeftParam ~ P(space ~ methodImplLeftParam).rep(0))

  def methodImplLeftParam[_: P] = P(Lexical.identifier | arrayPatternMatch | listPatternMatch | bracketPatternMatch)
  //P(bracketPatternMatch | emptyArray | Lexical.identifier)

  def methodImplWhere[_: P] = P( &("where") ~ "where" ~ optSpace ~ newLine ~ optSpace ~
    methodDecl ~ optSpace ~ newLine ~ multiplePatternMatches).map(f => {
    Method(f._1, f._2)
  })

  def multiplePatternMatches[_: P] = P(optSpace ~ patternMatch ~ optSpace ~ newLine).rep(1)

  def myMethodImpl[_: P] = methodImpl

  def isEmptyArray(s: String) = {
    (s.filterNot(_ == ' ') == "[]")
  }


  def methodCallIdentifiers[_: P] = P(methodCallIdentifier) ~ P(!(space ~ "where") ~ space ~ methodCallParameter).rep(0)
  //def methodCallIdentifiers2[_: P] = P(methodCallIdentifier) ~ P(space ~ methodCallParameter).rep(0)
  def methodCallIdentifier[_: P] = P(Lexical.identifier)
  def methodCallParameter[_: P] = P(!P("where") ~ P(emptyArray | bracketedMethodCallParameter | Lexical.identifier))

  def bracketedMethodCallParameter[_: P] = P("(" ~ optSpace ~
    expression ~
    optSpace ~ ")")

  // head :: tail
  def headTailExpression[_: P] = P(Lexical.identifier ~ optSpace ~ "::" ~ optSpace ~ Lexical.identifier)
    .map(f => {
      MethodExpression(HeadTailExpression(), f._1, f._2)
    })

  // xs ++ [x]
  def listAppendExpression[_: P] = P(Lexical.identifier ~ optSpace ~ "++" ~ optSpace ~ "[" ~ optSpace ~ Lexical.identifier ~ optSpace ~ "]")
    .map(f => {
      MethodExpression(ListAppendExpression(), f._1, f._2)
    })

  def expression[_: P] = P(headTailExpression | listAppendExpression)

  /*
  methodCall
   */

  // eg. Nat
  def isBuildInIdrisType(t: Identifier): Boolean = {
    t.name.head.isUpper
  }












  def methodImplRightSideDataValue[_: P]: P[DataValueOrMethodCall] = P(&(Lexical.uppercase) ~ Lexical.identifier ~ P(&(space ~ Lexical.uppercase) ~ space ~ Lexical.identifier).rep(0)).map(f => {
    if (isBuildInIdrisType(f._1)) {
      DataValueOrMethodCall(Some(DataValue(f._1, f._2)), None)
    } else {
      DataValueOrMethodCall(None, Some(MethodCall(f._1, f._2, isDataResultNotMethodCall = true)))
    }
  })

  def methodImplRightSideEmptyArrayDataValue[_ : P]: P[DataValueOrMethodCall] = P(emptyArray).map((f) => {
    DataValueOrMethodCall(Some(DataValue(Identifier(f.name) ,Seq.empty)), None)
  })

  // Calling a method
  def methodImplRightSideMethodCall[_: P]: P[DataValueOrMethodCall] = P ( methodCallIdentifiers ).map(f => DataValueOrMethodCall ( None, Some( MethodCall(f._1, {
    for (n <- f._2.toList) yield {
      n match {
        case Identifier(s) => Identifier(s)
        case ArrayIdentifier(s, isEmpty) => {
          ArrayIdentifier(s, isEmptyArray(s))
        }
        case _ => n
      }
    }
  }))))

  def methodImplRightSideSuccessorCall[_: P] = P ( "S" ~ optSpace ~ "(" ~ optSpace ~ methodImplRightSideMethodCall ~ optSpace ~ ")" ).map(f => {
    f.copy(methodCall = Some(f.methodCall.get.copy(isSuccessorMethodCall = true))) // @todo Replace with
  })

  def methodImplRightSide[_: P] = P(methodImplRightSideSuccessorCall | methodImplRightSideEmptyArrayDataValue | methodImplRightSideDataValue | methodImplRightSideMethodCall)









  def patternMatch[_: P]: P[MethodLine] = methodImpl
  /*
  P(Lexical.identifier ~ patternMatchIdentifiers ~ optSpace ~ "=" ~ optSpace ~ patternMatchRightSide).map(f => {
  $$$$
  MethodLine2(f._1, f._2, f._3)
}).log(*/
  //  def patternMatchIdentifiers[_: P] = patternMatchIdentifier.rep
  //  def patternMatchIdentifier[_: P] = P(space ~ !("=") ~ P(Lexical.identifier | arrayPatternMatch | listPatternMatch | bracketPatternMatch))

  def arrayPatternMatch[_: P] = emptyArray

  def listPatternMatch[_: P] = P("(" ~ optSpace ~ Lexical.identifier ~ optSpace ~ "::" ~ optSpace ~ Lexical.identifier ~ ")")
    .map(f => MethodExpression(HeadTailExpression(), f._1, f._2))

  def bracketPatternMatch[_: P] = P(&("(") ~ "(" ~ optSpace ~ Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0) ~ optSpace ~")")
    .map(f => Bracketed(f._1, f._2))

  def identifiers[_: P] = P(Lexical.identifier ~ P(space ~ Lexical.identifier).rep(0))




  def identifier_t[_: P] = Lexical.identifier

  def optSpace[_: P] = CharIn(" ").rep(0)
  def space[_: P] = CharIn(" ").rep(1)

  def moduleHeader[_: P] = P( "module" ~ space ~ identifier_t ~ optSpace ~ ";" )

  //-----------------------------

  def importLine[_: P] = P ( "import" ~ space ~ identifier_t ~ optSpace ~ ";" )


  // Matching an empty array such as [] or [ ]
  def emptyArray[_: P] = P ( "[" ~ optSpace ~ "]" ).map(f => ArrayIdentifier("[]", true))

  // Documentation

  def SingleLineComment_t[_: P] = P( "--" ~ CharsWhile(_ != '\n', 0) )
  def DocCommentLine[_: P] = P ( "|||" ~ CharsWhile(_ != '\n', 0) )
  def ArgCommentLine[_: P] = P ( "|||" ~ optSpace ~ "@" ~ CharsWhile(_ != '\n', 0) )

}

