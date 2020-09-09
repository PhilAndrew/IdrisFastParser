package fold.parse.idris.parse

import fastparse.Parsed
import Grammar.ParsedFile

object PostProcess {


  // The variables which are in scope
  def referencesInScope(): Seq[String] = ???

  // The functions which are in scope
  def functionsInScope(): Seq[String] = ???


  def postProcessParse(result: Parsed[ParsedFile]): Parsed[ParsedFile] = {

    import com.softwaremill.quicklens._


    result match {
      case Parsed.Success(file, index) => {
        if (file.method.isDefined) {
          val value = file.method.get
          if (value.patternMatch.head.methodImplWhere.isDefined) {
            val patterns = value.patternMatch.head.methodImplWhere.get.patternMatch
            val newPatterns = for (p <- patterns) yield {
              // @todo This is a bad heuristic, we are attempting to identify if this is a method call or
              // @todo something else, in this positive case we assume something else
              if (p.statement.dataValue.isDefined) {
                println("Case")
                p
              } else {
                val m = p.statement.methodCall
                if (p.statement.methodCall.get.parameter.isEmpty) {
                  p.modify(_.statement.methodCall.each.isReferenceNotMethodCall).setTo(true)
                } else p
              }


            }
            val value2 = value.modify(_.patternMatch.at(0).methodImplWhere.each.patternMatch).setTo(newPatterns)
            val file2 = file.modify(_.method).setTo(Some(value))
            Parsed.Success(file2, index)
          } else {
            Parsed.Success(file, index)
          }
        } else result
      }
      case _ => result
    }
  }


}
