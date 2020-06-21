package fold.parse.idris

import java.nio.file.{Files, Path}

import fastparse.Parsed
import fold.parse.idris.Grammar.{ArrayIdentifier, Extraction, Identifier}

object TypeScript {

  case class BracketedBinding(origional: Seq[String], // Assume ["S", "k"] which is (S k)
                             name: String,            // Assume "k" as any place this name is found we are referring to this backeted binding
                             substitution: String     // Assume "k-1" to substitute into this place
                             )
  case class Binding(localName: String, origionalName: Option[String], typeOf: String, bracketedBinding: Option[BracketedBinding]) // @todo bracketedBinding set to None
  case class ParameterBinding(bindings: Seq[Binding])

  case class CodeGenerationPreferences(usePreludeTs: Boolean = true,
                                       usePreludeTsVectorForList: Boolean = true,
                                       usePreludeTsListForList: Boolean = false,
                                       placeFunctionsIntoClasses: Boolean = false,
                                       codeGenerationDebugComments: Boolean = false) {
    def listType() = if (usePreludeTsVectorForList) "Vector" else if (usePreludeTsListForList) "LinkedList" else ""
  }

  case class LocalVariable(methodName: String, variableName: String, typeOf: String, indexInMethod: Int, variableAlias: Option[String])

  case class CodeEnvironment(scopedVariablePrefix: Option[String] = None,
                             localVariablesFromMethodParameterScope: Seq[LocalVariable] = Seq.empty,
                             generationPreferences: CodeGenerationPreferences = CodeGenerationPreferences())
  case class CodeLine(line: String, indentLevel: Int = 0)

  // Use linked list
  // https://github.com/immutable-js/immutable-js

  def basicTypeToTypescript(code: CodeGenerationPreferences, t: String, ft: String): String = {
    t match {
      case "List" => {
        s"${code.listType()}<${ft}>"
      }
      case "Bool" => "boolean"
      case _ => t
    }
  }

  def functionTypeParameters(value: Grammar.Method) = {
    if (value.methodDefinition.parameters.head.rest.isEmpty) "" else
    s"${value.methodDefinition.parameters.head.rest.head.name}"
  }

  def emptyList(c: CodeGenerationPreferences) = if (c.usePreludeTs) {
    if (c.usePreludeTsVectorForList)
      "Vector.of()"
    else if (c.usePreludeTsListForList)
      "LinkedList.of()"
  } else {
    "@todo ERROR"
  }

  def methodCall(code: CodeGenerationPreferences, methodCall: Grammar.MethodCall) = {
    if (methodCall.isReferenceNotMethodCall)
      s"  return ${methodCall.method}"
    else {
      val params = for (p <- methodCall.parameter) yield {
        p match {
          case i: Identifier => i.name
          case a: ArrayIdentifier => {
            if (a.isEmpty)
              emptyList(code)
            else {
              throw new Exception("Error")
              ""
            }
          }
        }
      }
      s"  return ${methodCall.method.name}(${params.mkString(", ")})"
    }
  }

  def prefix(codeEnvironment: CodeEnvironment, name: String) = {
    if (codeEnvironment.scopedVariablePrefix.isDefined)
      s"${codeEnvironment.scopedVariablePrefix.get}${name.capitalize}"
    else
      name
  }

  def buildExtractor(codeEnvironment: CodeEnvironment, patternMatch: Grammar.PatternMatch): Seq[CodeLine] = {
    (for (r <- patternMatch.rest.zipWithIndex) yield {
      r._1 match {
        case e: Extraction => {
          Seq(CodeLine(s"const ${prefix(codeEnvironment, e.first.name)} = head${codeEnvironment.generationPreferences.listType()}(param${r._2+1})"),
             CodeLine(s"const ${prefix(codeEnvironment, e.second.name)} = tail${codeEnvironment.generationPreferences.listType()}(param${r._2+1})"))
        }
        case i: Identifier => {
          // What is current method name?
          // What is current index?
          // What is identifier name?
          val currentMethodName = patternMatch.first.name // @todo not sure if this is true
          val index = r._2
          val identifierName = i.name

          // Find in codeEnvironment
          val found = codeEnvironment.localVariablesFromMethodParameterScope.find(f => { (f.methodName == currentMethodName) && (f.indexInMethod == index) && (f.variableName == identifierName) })
          if (found.isDefined) {
            Seq(CodeLine(s"const ${identifierName} = ${found.get.variableAlias.getOrElse("?what")}"))
          } else
            Seq(CodeLine("?notFound"))
        }
        case _ => Seq.empty
      }
    }).flatten
  }

  def codeLinesToString(indent: Int, lines: Seq[CodeLine]): String = {
    (for (l <- lines) yield {
      "  ".repeat(indent + l.indentLevel).mkString + l.line
    }).mkString("\n")
  }

  def emptyCodeLine() = CodeLine("")

  def buildPatternMatchCondition(codeEnvironment: CodeEnvironment, patternMatch: Grammar.PatternMatch): Seq[CodeLine] = {
    val lines: Seq[Option[String]] = for (r <- patternMatch.rest.zipWithIndex) yield {
      val i: Option[String] = r._1 match {
        case a: ArrayIdentifier => {
          if (a.isEmpty)
            Some(s"(param${r._2+1}.isEmpty())")
          else {
            // @todo This one in cases of non empty array
            None
          }
        }
        case _ => None
      }
      i
    }

    val conditions: Seq[String] = lines.flatten
    if (conditions.isEmpty) Seq(CodeLine("{"))
    else Seq(CodeLine(s"if ${if (conditions.size == 1) {
      conditions.head
    } else {
      conditions.mkString("(", " && ", ")")
    }
    } {"))
  }

  def indented(code: Seq[CodeLine], indentLevel: Int): Seq[CodeLine] = {
    for (c <- code) yield {
      c.copy(indentLevel = indentLevel)
    }
  }

  def declaredIdentifier(codeEnvironment: CodeEnvironment, name: String): String = {
    val found = codeEnvironment.localVariablesFromMethodParameterScope.find(_.variableName == name)
    if (found.isDefined)
      name
    else
      "?notFoundIdentifier"
  }

  def buildCode(codeEnvironment: CodeEnvironment, patternMatch: Grammar.PatternMatch): Seq[CodeLine] = {

    if (patternMatch.methodCall.isReferenceNotMethodCall)
      Seq(CodeLine(s"return ${patternMatch.methodCall.method.name}"))
    else {
      val p = for (p <- patternMatch.methodCall.parameter) yield {
        p match {
          case i: Identifier => {
            declaredIdentifier(codeEnvironment, i.name)
          }
          case e: Extraction => {
            // Do prepend
            s"${e.second.name}.prepend(${e.first.name})"
          }
          case _ => ""
        }
      }

      val parameters = p.mkString(", ")
      Seq(CodeLine(s"return ${patternMatch.methodCall.method.name}(${parameters})"))
    }
  }

  def updateCodeEnvironment(codeEnvironment: CodeEnvironment, patternMatch: Grammar.PatternMatch): CodeEnvironment = {
    // @todo Add the aliases
    val methodName = patternMatch.first.name
    // Add variables to the codeEnvironment
    val add: Seq[LocalVariable] = (for (p <- patternMatch.rest.zipWithIndex) yield {
      p._1 match {
        case i: Identifier => {
          Some(LocalVariable(methodName, i.name, "?unknownTypeOf", p._2, Some(s"param${p._2+1}"))) // @todo fix variable alias param
        }
        case _ => None
      }
    }).flatten

    val l: Seq[LocalVariable] = codeEnvironment.localVariablesFromMethodParameterScope ++ add
    codeEnvironment.copy(localVariablesFromMethodParameterScope = l)
  }

  def patternMatchesToCode(codeEnvironment: CodeEnvironment, methodImplWhere: Grammar.MethodImplWhere): Seq[CodeLine] = {

    val codeLines: Seq[Seq[CodeLine]] = for (m <- methodImplWhere.patterns.zipWithIndex) yield {

      val codeEnvironmentNested: CodeEnvironment = updateCodeEnvironment(codeEnvironment, m._1)

      val p = buildPatternMatchCondition(codeEnvironmentNested, m._1)
      val b = buildExtractor(codeEnvironmentNested, m._1)
      val c = buildCode(codeEnvironmentNested, m._1)

      val joined: Seq[CodeLine] = p ++ indented(b ++ c, 1) ++ Seq({
        if (m._2 == (methodImplWhere.patterns.size-1))
          CodeLine("}")
        else
          CodeLine("} else")
      })
      if (codeEnvironmentNested.generationPreferences.codeGenerationDebugComments) {
        CodeLine(s"// Pattern matching function ${m._2+1}") +: joined
      } else joined
    }

    // Insert an empty line between each group
    val deGrouped: Seq[CodeLine] = codeLines.filter(_.nonEmpty).zipWithIndex.flatMap(f => {
      if (f._2 == 0) {
        f._1
      } else {
        f._1
        //CodeLine("") +: f._1
      }
    })

    deGrouped
  }

  def methodDefinition(methodImplWhere: Option[Grammar.MethodImplWhere], code: CodeGenerationPreferences, c: CodeEnvironment) = {
    if (methodImplWhere.isDefined) {
      val last = methodImplWhere.get.methodDefinition.parameters.last
      val r = methodImplWhere.get.methodDefinition.parameters.dropRight(1)
      val paramTypes = for (p <- r) yield p.firstParam.name
      val ft = "a"
      val paramNames = paramTypes.zipWithIndex.map((t: (String, Int)) => s"param${t._2 + 1}")
      val param = paramTypes.zipWithIndex.map((t: (String, Int)) => s"${paramNames(t._2)}: ${basicTypeToTypescript(code, t._1, "a")}").mkString(", ")

      val methodBody: String = (for (p <- methodImplWhere.get.patterns) yield {
        p.toString
      }).mkString("\n")

      val what = methodImplWhere.get.patterns(1).rest.toString

      val what2 = codeLinesToString(2, patternMatchesToCode(c, methodImplWhere.get))

      // @todo The function is parameterized by <${ft}> but I deleted that to make it work
      s"""  function ${methodImplWhere.get.methodDefinition.name}($param): ${basicTypeToTypescript(code, last.firstParam.name, ft)} {
         |${what2}
         |}""".stripMargin
    } else ""
  }

  def docComments(code: CodeGenerationPreferences, params: Seq[(String, String)]): String = {
    val codeLines = (for (p <- params.zipWithIndex) yield {
      CodeLine(s"* @param ${p._1._1} ${code.listType()}<a> ?")
    }) :+ CodeLine(s"* @returns ?")

    val parameterJsDoc = codeLinesToString(0, codeLines)

    s"""/**
     |* ?
     |${parameterJsDoc}
     |*/""".stripMargin
  }

  def generateLocalVariables(methodName: String, parameterNames: Seq[String], parameterTypes: Seq[String]): Seq[LocalVariable] = {
    for (n <- parameterNames.zip(parameterTypes).zipWithIndex) yield {
      LocalVariable(methodName, n._1._1, n._1._2, n._2, None)
    }
  }

  def isDataType(name: String) = {
    name.head.isUpper
  }

  //ParameterBinding
  def parameterBinding(method: Grammar.Method, methodLine: Grammar.MethodLine): ParameterBinding = {
    val totalParameters = method.methodDefinition.parameters.size
    val methodDef = method.methodDefinition.parameters
    val methodLineParameters = methodLine.left.rest


    val parameterNames = for (p <- (0 until (totalParameters-1))) yield {
      s"functionParameter${p+1}"
    }

    val bindings = for (p <- parameterNames.zip(methodLineParameters).zip(methodDef)) yield {
      // @todo Bracketed binding
      Binding(p._1._1, if (p._1._2.name.isEmpty) None else if (isDataType(p._1._2.name.get)) None else Some(p._1._2.name.get), p._2.firstParam.name, None)
    }
    ParameterBinding(bindings)
  }

  def toTypescriptAST(fileName: String, idrisAst: Parsed[Grammar.Method], code: CodeGenerationPreferences) = {
    idrisAst match {
      case Parsed.Success(value, index) => {
        if (value!=null) {
          for (m <- value.methodLine)
            parameterBinding(value, m)

          val parameterTypes = for (p <- value.methodDefinition.parameters) yield (p.firstParam.name)

          val methodLines = value.methodLine.size
          println(methodLines)

          // In the case of multiple method lines then the variable names maybe different so we need to use
          // some common name

          val parameterNames = for (p <- value.methodLine.head.left.rest) yield (p.name.getOrElse(""))

          val parameterNamesFiltered: Seq[Option[String]] = parameterNames.map((f) => {
            if (f.size == 0) None else if (f.head.isUpper == false) Some(f) else None
          })

          val codeEnvironment = CodeEnvironment(localVariablesFromMethodParameterScope = generateLocalVariables(value.methodDefinition.name, parameterNames, parameterTypes),
            generationPreferences = code)

          val ft = functionTypeParameters(value)

          val params = parameterNamesFiltered.zip(parameterTypes).map(f => {
            if (f._1.isDefined) Some((f._1.get, f._2)) else None
          }).flatten

          val parametersStr = for (p <- params) yield (p._1 + ": " + basicTypeToTypescript(code, p._2, ft))

          val header =
            s"""// npm install --save prelude-ts
              |import { Vector, LinkedList } from "prelude-ts";
              |
              |function head${code.listType()}<a>(param: ${code.listType()}<a>): a {
              |  return param.head().getOrThrow()
              |}
              |
              |function tail${code.listType()}<a>(param: ${code.listType()}<a>): ${code.listType()}<a> {
              |  return param.tail().getOrElse(${emptyList(code)})
              |}
              |
              |""".stripMargin

          val methodImpl = methodCall(code, value.methodLine.head.methodCall)
          val methodDef = methodDefinition(value.methodLine.head.methodImplWhere, code, codeEnvironment)
          val functionDoc = docComments(code, params)

          val parameterized = if (ft.trim.isEmpty) "" else s"<$ft>"

          val function = s"""${functionDoc}
                            |export function ${value.methodDefinition.name}${parameterized}(${parametersStr.mkString(", ")}): ${basicTypeToTypescript(code, parameterTypes.last, ft)}
             |{
             |${methodDef}
             |${methodImpl}
             |}""".stripMargin

          val output = header + function

          Files.writeString(Path.of("typescript/src/test/" + fileName), output)

          output
        } else {
          Files.writeString(Path.of("typescript/src/test/" + fileName), "failure case")
          ""
        }
      }


      case _ => {
        ""
      }
    }
  }

}
