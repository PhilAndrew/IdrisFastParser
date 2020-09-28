package fold.parse.idris.para

object ParaGrammar {

  import fastparse.NoWhitespace._
  import fastparse._

  import ParaLexical._

  case class Identifier(name: String)
  case class FunctionType(identifier: Identifier)
  case class FunctionDefinition(name: Identifier, functionTypes: Seq[FunctionType])

  // average of str is cast total length / cast num words
  case class FunctionParameter(identifier: Identifier)
  case class FunctionImplementation(functionName: Seq[Identifier], parameter: FunctionParameter, implementation: String)

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

  def sentenceTo2[_: P]: P[Seq[Identifier]] = P(
    P(&("\"") ~ P("\"The average word length is:\"")) |
      P(ofAndIdentifier ~
      P(space ~ ofAndIdentifier).rep(0))).map((f) => {
    f match {
      case t: Tuple2[Identifier, Seq[Identifier]] => {
        Seq[Identifier](t._1) ++ t._2
      }
      case _ => Seq.empty
    }
  })

  def functionTo(f: Seq[_]): Seq[FunctionType] = {
    // Drop all "to"'s
    (for (x <- f) yield {
      val name = x match {
        case i:Identifier => i.name
        case _ => ""
      }
      if (name == "to")
        None
      else
        Some(FunctionType(Identifier(name)))
    }).flatten
  }

  // average is string to double
  def sentenceIs[_: P] = P(
    // something is something to something to something
    &(P(identifier ~ space).rep(1) ~ "is" ) ~ P(
      P(P(identifier ~ space).rep(1) ~ "is" ~ space ~ sentenceTo2)
    ) ~ End
  ).map((f) => {
    val functionName: Seq[Identifier] = f._1
    val functionTypes: Seq[Identifier] = f._2
    FunctionDefinition(functionName.head, functionTo(functionTypes))
  })

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
  def sentenceImpl[_: P]: P[Seq[_]] = {
    P(some ~ P(space ~ some).rep(0)).map((f) => {
      Seq(f._1) ++ f._2
    })
  }

  def sentenceOf[_: P] = P(
    // word count of str is the length of words of str
    &(P(identifier ~ space).rep(1) ~ "of") ~
      P(P(identifier ~ space).rep(1) ~ "of" ~ space ~ P(identifier ~ space).rep(1) ~ P(&("is") ~ "is" ~ space ~ sentenceImpl).rep(0) ~ End)
  ).map((f) => {

    FunctionImplementation(f._1, FunctionParameter(f._2.head), "")
  })

  def comment[_: P] = P(&("#") ~ P("#" ~ AnyChar.rep(0) ~ End))

  def sentence[_: P] = P(
    // something is something to something to something
    comment |
    sentenceIs | sentenceTo | sentenceOf
  ).map((f) => {
    f match {
      case x: FunctionDefinition => {
        x
      }
      case _ => f
    }
  } )

  // average of str is cast total length / cast num words

  def optSpace[_: P] = CharIn(" ").rep(0)
  def space[_: P] = CharIn(" ").rep(1)


}

