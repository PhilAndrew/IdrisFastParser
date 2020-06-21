package fold.parse.idris

import fastparse.Parsed
import fold.parse.idris.Grammar.Method

object PostProcess {


  // The variables which are in scope
  def referencesInScope(): Seq[String] = ???

  // The functions which are in scope
  def functionsInScope(): Seq[String] = ???

  def postProcessParse(result: Parsed[Method]): Parsed[Method] = {
    import com.softwaremill.quicklens._
    result match {
      case Parsed.Success(value, index) => {
        if (value.methodLine.head.methodImplWhere.isDefined) {
          val patterns = value.methodLine.head.methodImplWhere.get.patterns
          val newPatterns = for (p <- patterns) yield {
            // @todo This is a bad heuristic, we are attempting to identify if this is a method call or
            // @todo something else, in this positive case we assume something else
            if (p.methodCall.parameter.isEmpty) {
              p.modify(_.methodCall.isReferenceNotMethodCall).setTo(true)
            } else p
          }
          Parsed.Success(value.modify(_.methodLine.at(0).methodImplWhere.each.patterns).setTo(newPatterns), index)
        } else {
          Parsed.Success(value, index)
        }
      }
      case _ => result
    }
  }


}