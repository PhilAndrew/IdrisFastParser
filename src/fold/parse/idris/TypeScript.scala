package fold.parse.idris

import java.nio.file.{Files, Path}

import fastparse.Parsed
import fold.parse.idris.Grammar.{ArrayIdentifier, Bracketed, Extraction, Identifier, Method}
import better.files._
import File._
import java.io.{File => JFile}

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
                                       codeGenerationDebugComments: Boolean = false,
                                       useNodeJSLibraryOffensive: Boolean = true,
                                       useSingleQuotesElseDoubleQuotes: Boolean = true,
                                       useTripleEqualsForIntegerComparisons: Boolean = true,
                                       generateNodeJSProject: Boolean = true,
                                        outputPath: Option[String] = None) {
    def listType() = if (usePreludeTsVectorForList) "Vector" else if (usePreludeTsListForList) "LinkedList" else ""
  }

  case class LocalVariable(methodName: String, variableName: String, typeOf: String, indexInMethod: Int, variableAlias: Option[String])

  case class CodeEnvironment(scopedVariablePrefix: Option[String] = None,
                             localVariablesFromMethodParameterScope: Seq[LocalVariable] = Seq.empty,
                             generationPreferences: CodeGenerationPreferences = CodeGenerationPreferences())

  // For example
  // winston
  // https://www.npmjs.com/package/winston
  // npm install --save winston
  // import * as winston from 'winston'
  case class NodeJsLibrary(name: String, url: String, npmInstall: String, imports: Seq[String])

  case class PartialCodeLine(code: String, nodeJsLibraryUsage: Seq[NodeJsLibrary] = Seq.empty)
  case class CodeLine(line: Seq[PartialCodeLine], indentLevel: Int = 0, nodeJsLibraryUsage: Seq[NodeJsLibrary] = Seq.empty)

  val preludeTsVectorForList = NodeJsLibrary("Vector", "https://github.com/emmanueltouzery/prelude-ts", "prelude-ts", Seq("import Vector from 'prelude-ts'"))
  val preludeTsListForList = NodeJsLibrary("LinkedList", "https://github.com/emmanueltouzery/prelude-ts", "prelude-ts", Seq("import LinkedList from 'prelude-ts'"))
  val assertNodeJsLibrary = NodeJsLibrary("Offensive", "https://www.npmjs.com/package/offensive", "offensive",
    Seq("import 'offensive/assertions/length'",
    "import 'offensive/assertions/anInteger'",
    "import 'offensive/assertions/greaterThanOrEqualTo'",
    "import check from 'offensive'"))

  // Use linked list
  // https://github.com/immutable-js/immutable-js

  /**
   * Converts an Idris type to an equivalent Typescript type
   * @param preferences
   * @param idrisType
   * @param parameterizedType
   * @return
   */
  def idrisTypeToTypescriptType(preferences: CodeGenerationPreferences,
                                idrisType: String,
                                parameterizedType: String): PartialCodeLine = {
    idrisType match {
      case "List" => {
        PartialCodeLine(s"${preferences.listType()}<${parameterizedType}>")
      }
      case "Bool" => PartialCodeLine("boolean")
      case "Nat" => PartialCodeLine("number")
      case "True" => PartialCodeLine("true")
      case "False" => PartialCodeLine("false")
      case "Type" => PartialCodeLine("string")
      case _ => PartialCodeLine(idrisType)
    }
  }

  def constantOf(name: String): Option[String] = {
    if (name.equals("True"))
      Some("true")
    else if (name.equals("False"))
      Some("false")
    else None
  }

  def emptyListInstance(preferences: CodeGenerationPreferences): PartialCodeLine = {
    if (preferences.usePreludeTs) {
      if (preferences.usePreludeTsVectorForList)
        PartialCodeLine("Vector.of()", Seq(preludeTsVectorForList))
      else if (preferences.usePreludeTsListForList)
        PartialCodeLine("LinkedList.of()", Seq(preludeTsListForList))
      else // @todo Handle this case
        PartialCodeLine("LinkedList.of()", Seq(preludeTsListForList))
    } else {
      // @todo Handle this case
      PartialCodeLine("LinkedList.of()", Seq(preludeTsListForList))
    }
  }

  def defaultCodeLine(line: String): CodeLine = {
    CodeLine(Seq(PartialCodeLine(line)))
  }

  def isCapitalized(name: String): Boolean = {
    if (name.size == 0)
      false
    else
      name.head.isUpper
  }

  def nodeJsLibraryOf(preferences: CodeGenerationPreferences): Seq[NodeJsLibrary] = {
    if (preferences.usePreludeTsListForList)
      Seq(preludeTsListForList)
    else
      Seq(preludeTsVectorForList)
  }

  def partialCodeLine(str: String): Seq[PartialCodeLine] = {
    Seq(PartialCodeLine(str))
  }

  def buildExtractor(preferences: CodeGenerationPreferences, codeEnvironment: CodeEnvironment, patternMatch: Grammar.MethodLine): Seq[CodeLine] = {
    (for (r <- patternMatch.left.rest.zipWithIndex) yield {
      if (r._1.extractionForm.isDefined) {
        val e = r._1.extractionForm.get

        Seq(
          CodeLine(partialCodeLine(s"const ${prefix(codeEnvironment, e.first.name)} = head${codeEnvironment.generationPreferences.listType()}(${patternMatch.left.methodName.name}Param${r._2 + 1})"), 0, nodeJsLibraryOf(preferences)),
          CodeLine(partialCodeLine(s"const ${prefix(codeEnvironment, e.second.name)} = tail${codeEnvironment.generationPreferences.listType()}(${patternMatch.left.methodName.name}Param${r._2 + 1})"), 0, nodeJsLibraryOf(preferences)))
      } else if (r._1.bracketedForm.isDefined) {
        val b: Bracketed = r._1.bracketedForm.get
        if (b.first.name == "S") { // Assume (S k)
          Seq(defaultCodeLine(s"const k = ${patternMatch.left.methodName.name}Param${r._2+1} - 1"))
        } else
          Seq.empty[CodeLine]
      } else {
        Seq.empty[CodeLine]
      }

      /*r._1.extractionForm match {
        case e: Extraction => {
          Seq(CodeLine(s"const ${prefix(codeEnvironment, e.first.name)} = head${codeEnvironment.generationPreferences.listType()}(${patternMatch.left.methodName.name}Param${r._2 + 1})"),
            CodeLine(s"const ${prefix(codeEnvironment, e.second.name)} = tail${codeEnvironment.generationPreferences.listType()}(${patternMatch.left.methodName.name}Param${r._2 + 1})"))
        }
        case i: Identifier => {
          // What is current method name?
          // What is current index?
          // What is identifier name?
          val currentMethodName = patternMatch.left.methodName.name // @todo not sure if this is true
          val index = r._2
          val identifierName = i.name

          // Find in codeEnvironment
          val found = codeEnvironment.localVariablesFromMethodParameterScope.find(f => {
            (f.variableAlias.contains(identifierName))
          })
          //(f.methodName == currentMethodName) &&
          if (found.isDefined) {
            Seq(CodeLine(s"const ${identifierName} = ${found.get.variableName}"))
          } else
            Seq(CodeLine("?notFound"))
        }
        case _ => Seq.empty
      }*/
    }).flatten
  }

  def codeLinesToString(indent: Int, lines: Seq[CodeLine]): String = {
    val i = if (lines.size == 1) indent -1 else indent
    (for (l <- lines) yield {
      "  ".repeat(i + l.indentLevel).mkString + toTypescript2(l.line)
    }).mkString("\n")
  }

  def emptyCodeLine() = defaultCodeLine("")

  def buildPatternMatchCondition(code: CodeGenerationPreferences,
                                 codeEnvironment: CodeEnvironment,
                                 patternMatch: Grammar.MethodLine): Seq[CodeLine] = {

    val lines: Seq[Option[String]] = for (rr <- patternMatch.left.rest.zipWithIndex) yield {
      val r = rr._1.name
      if (r.isDefined) {
        val param = s"${patternMatch.left.methodName.name}Param${rr._2 + 1}"
        if (r.get == "[]")
          Some(s"(${param}.isEmpty())")
        else
          if (r.get == "Z")
            Some("(" + integerComparison(code, param, 0) + ")")
          else
            if (r.get == "True")
              Some(s"(${param} === true)")
            else
              if (r.get == "False")
                Some(s"(${param} === false)")
              else
                None
      } else None
    }

    val conditions: Seq[String] = lines.flatten
    if (conditions.isEmpty) Seq.empty //Seq(CodeLine("{"))
    else Seq(defaultCodeLine(s"if ${
      if (conditions.size == 1) {
        conditions.head
      } else {
        conditions.mkString("(", " && ", ")")
      }
    } {"))


    /*if (patternMatch.methodImplWhere.isDefined) {
      val lines: Seq[Option[String]] = for (r <- patternMatch.methodImplWhere.get.patternMatch.zipWithIndex) yield {
        val i: Option[String] = r._1 match {
          case a: ArrayIdentifier => {
            if (a.isEmpty)
              Some(s"(param${r._2 + 1}.isEmpty())")
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
      else Seq(CodeLine(s"if ${
        if (conditions.size == 1) {
          conditions.head
        } else {
          conditions.mkString("(", " && ", ")")
        }
      } {"))
    } else Seq.empty*/
  }




  // 100% LINE -----------------------------------------------------------------------------------------








  def functionTypeParameters(value: Grammar.Method) = {
    if (value.methodDefinition.parameters.head.rest.isEmpty) "" else
    s"${value.methodDefinition.parameters.head.rest.head.name}"
  }

  def prefix(codeEnvironment: CodeEnvironment, name: String) = {
    if (codeEnvironment.scopedVariablePrefix.isDefined)
      s"${codeEnvironment.scopedVariablePrefix.get}${name.capitalize}"
    else
      name
  }

  def integerComparison(code: CodeGenerationPreferences, param: String, compareToken: Int): PartialCodeLine = {
    if (code.useTripleEqualsForIntegerComparisons)
      PartialCodeLine(s"${param} === ${compareToken.toString}")
    else
      PartialCodeLine(s"${param} == ${compareToken.toString}")
  }

  def indented(code: Seq[CodeLine], indentLevel: Int): Seq[CodeLine] = {
    for (c <- code) yield {
      c.copy(indentLevel = indentLevel)
    }
  }

  def declaredIdentifier(codeEnvironment: CodeEnvironment, name: String): String = {
    val found = codeEnvironment.localVariablesFromMethodParameterScope.find(_.variableAlias.contains(name))
    if (found.isDefined)
      name
    else
      "?notFoundIdentifier"
  }

  def localVariable(code: CodeEnvironment, name: String): Option[String] = {
    val found = code.localVariablesFromMethodParameterScope.find(_.variableAlias.contains(name))
    if (found.isDefined)
      Some(found.get.variableName)
    else
      None
  }

  def getScopedVariable(codeEnvironment: CodeEnvironment, name: String): Option[LocalVariable] = {
    codeEnvironment.localVariablesFromMethodParameterScope.find(_.variableAlias.contains(name))
  }

  def buildCode(code: CodeGenerationPreferences, codeEnvironment: CodeEnvironment, patternMatch: Grammar.MethodLine): Seq[CodeLine] = {

    val c2 = updateCodeEnvironment(codeEnvironment, patternMatch)

    def la(i: Identifier) = {
      if (isCapitalized(i.name)) {
        i.name
      } else {
        localVariable(c2, declaredIdentifier(c2, i.name)).getOrElse(i.name)
      }
    }

    if (patternMatch.statement.dataValue.isDefined) {
      val r = (for (i <- patternMatch.statement.dataValue.get.rest) yield i.name).mkString(" ")
      val i = if (r.size > 0) " " + r else ""

      val c = constantOf(patternMatch.statement.dataValue.get.name.name)
      if (c.isDefined)
        Seq(defaultCodeLine(s"""return ${c.get}"""))
      else
        Seq(defaultCodeLine(s"""return "${patternMatch.statement.dataValue.get.name.name}$i""""))
    } else
    if ((patternMatch.statement.methodCall.isDefined) && (patternMatch.statement.methodCall.get.isReferenceNotMethodCall))
      Seq(defaultCodeLine(s"return ${localVariable(c2, patternMatch.statement.methodCall.get.method.name).get}"))
    else {
      val p = for (p <- patternMatch.statement.methodCall.get.parameter) yield {
        p match {
          case i: Identifier => {
            la(i)
          }
          case e: Extraction => {
            // Do prepend
            s"${localVariable(c2, e.second.name).get}.prepend(${e.first.name})"
          }
          case a: ArrayIdentifier => {
            if (a.isEmpty)
              toTypescript(emptyListInstance(code))
            else {
              throw new Exception("Error")
              ""
            }
          }
          case _ => ""
        }
      }

      val parameters = p.mkString(", ")
      if (isCapitalized(patternMatch.statement.methodCall.get.method.name))
        Seq(CodeLine(Seq(PartialCodeLine("return "),
          idrisTypeToTypescriptType(code, patternMatch.statement.methodCall.get.method.name, ")"))))
      else {
        // If no parameters then it may be a scoped variable
        val scoped = getScopedVariable(codeEnvironment, patternMatch.statement.methodCall.get.method.name)
        if (p.isEmpty && scoped.isDefined) {
          Seq(defaultCodeLine(s"return ${scoped.get.variableName}"))
        } else
          Seq(defaultCodeLine(s"return ${patternMatch.statement.methodCall.get.method.name}(${parameters})"))
      }
    }
  }

  def updateCodeEnvironment(codeEnvironment: CodeEnvironment, patternMatch: Grammar.MethodLine): CodeEnvironment = {
    // @todo Add the aliases
    val methodName = patternMatch.left.methodName.name
    // Add variables to the codeEnvironment
    if (patternMatch.methodImplWhere.isDefined) {
      val add: Seq[LocalVariable] = (for (p <- patternMatch.methodImplWhere.get.patternMatch.zipWithIndex) yield {
        p._1 match {
          case i: Identifier => {
            Some(LocalVariable(methodName, i.name, "?unknownTypeOf", p._2, Some(s"param${p._2 + 1}"))) // @todo fix variable alias param
          }
          case _ => None
        }
      }).flatten

      val l: Seq[LocalVariable] = codeEnvironment.localVariablesFromMethodParameterScope ++ add
      codeEnvironment.copy(localVariablesFromMethodParameterScope = l)
    } else {
      val params = for (p <- patternMatch.left.rest.zipWithIndex) yield {
        if (p._1.name.isDefined) {
          if (p._1.name == "[]") {
            None
          } else {
            Some(p._1.name.get)
          }
        } else {
          None
        }
      }

      val addLocalVariables = (for (p <- params.zipWithIndex) yield {
        val prefix = s"${patternMatch.left.methodName.name}Param${p._2+1}"
        if (p._1.isDefined)
          Some(LocalVariable(methodName = patternMatch.left.methodName.name, variableName = prefix, typeOf = "unknown", indexInMethod = p._2, variableAlias = p._1))
        else
          None
      }).flatten

      codeEnvironment.copy(localVariablesFromMethodParameterScope = addLocalVariables ++ codeEnvironment.localVariablesFromMethodParameterScope)
    }
  }


  def buildExtractorLocalVariables(codeEnvironmentNested: CodeEnvironment, methodLine: Grammar.MethodLine): CodeEnvironment = {
    val found: Seq[Option[Seq[LocalVariable]]] = for (r <- methodLine.left.rest) yield {

      if (r.extractionForm.isDefined) {
        // methodName: String, variableName: String, typeOf: String, indexInMethod: Int, variableAlias: Option[String]
        val s = Seq(LocalVariable(methodName = methodLine.left.methodName.name, variableName = r.extractionForm.get.first.name, typeOf = "", indexInMethod = 0, variableAlias = Some(r.extractionForm.get.first.name)),
          LocalVariable(methodName = methodLine.left.methodName.name, variableName = r.extractionForm.get.second.name, typeOf = "", indexInMethod = 0, variableAlias = Some(r.extractionForm.get.second.name)))
        Some(s)
      } else None
    }

    val flat : Seq[LocalVariable] = found.flatten.flatten

    codeEnvironmentNested.copy(localVariablesFromMethodParameterScope = flat ++ codeEnvironmentNested.localVariablesFromMethodParameterScope)
  }

  // @todo Merge the next two functions
  def patternMatchesToCode(code: CodeGenerationPreferences, codeEnvironment: CodeEnvironment, method: Grammar.Method): Seq[CodeLine] = {

    val what = for (m <- method.patternMatch.zipWithIndex) yield {

      val codeEnvironmentNested: CodeEnvironment = updateCodeEnvironment(codeEnvironment, m._1)

      val p = buildPatternMatchCondition(code, codeEnvironmentNested, m._1)
      val b = buildExtractor(code, codeEnvironmentNested, m._1)

      val codeEnvironmentExtractor = buildExtractorLocalVariables(codeEnvironmentNested, m._1)
      (m._1, m._2, codeEnvironmentNested, p, b, codeEnvironmentExtractor)
    }

    val codeLines: Seq[Seq[CodeLine]] = for (m <- what) yield {
      val codeEnvironmentNested: CodeEnvironment = m._3
      val p = m._4
      val b = m._5
      val codeEnvironmentExtractor = m._6

      // @todo At this point at buildExtractor above we need to copy new codeEnvironmentNested variables into it
      val codeBuilt = buildCode(code, codeEnvironmentExtractor, m._1)

      val joined: Seq[CodeLine] = if (p.isEmpty) {
        p ++ indented(b ++ codeBuilt, 0)
      }
      else {
        p ++ indented(b ++ codeBuilt, 1)
      }

      val result = if (codeEnvironmentExtractor.generationPreferences.codeGenerationDebugComments) {
        defaultCodeLine(s"// Pattern matching function ${m._2+1}") +: joined
      } else joined

      if (p.isEmpty) result else result ++ Seq(defaultCodeLine("}"))
    }
    val hasDefaultMatch = what.exists(_._4.isEmpty)

    val result: Seq[Seq[CodeLine]] = if (hasDefaultMatch) {
      codeLines
    } else {
      codeLines ++ Seq(Seq(defaultCodeLine("""return """"")))
    }

    // Insert an empty line between each group
    val deGrouped: Seq[CodeLine] = result.filter(_.nonEmpty).zipWithIndex.flatMap(f => {
      if (f._2 == 0) {
        f._1
      } else {
        f._1
        //CodeLine("") +: f._1
      }
    })
    deGrouped
  }


  def codeLineAssert(code: CodeGenerationPreferences, c: String): CodeLine = {
    CodeLine(partialCodeLine(c), 0, nodeJsLibraryOf(code))
  }

  def methodAssertions(code: CodeGenerationPreferences, paramNames: Seq[String], paramTypes: Seq[String]): Seq[CodeLine] = {
    val result = for (p <- paramNames.zip(paramTypes)) yield {
      p._2 match {
        case "Nat" => {
          Seq(codeLineAssert(code, s"""check(${p._1}, ${quoteString(code, p._1)}).is.anInteger()"""),
            codeLineAssert(code, s"""check(${p._1}, ${quoteString(code, p._1)}).is.greaterThanOrEqualTo(0)()"""))
        }
        case _ => Seq.empty
      }
    }
    result.flatten.toSeq
  }

  def methodDefinition(methodImplWhere: Option[Grammar.Method], code: CodeGenerationPreferences, c: CodeEnvironment): Option[String] = {
    if (methodImplWhere.isDefined) {
      val last = methodImplWhere.get.methodDefinition.parameters.last
      val r = methodImplWhere.get.methodDefinition.parameters.dropRight(1)
      val paramTypes = for (p <- r) yield p.param.name
      val ft = "a"
      val paramNames = paramTypes.zipWithIndex.map((t: (String, Int)) => s"${methodImplWhere.get.methodDefinition.name}Param${t._2 + 1}")
      val param = paramTypes.zipWithIndex.map((t: (String, Int)) => s"${paramNames(t._2)}: ${idrisTypeToTypescriptType(code, t._1, ft)}").mkString(", ")

      val methodBody: String = (for (p <- methodImplWhere.get.patternMatch) yield {
        p.toString
      }).mkString("\n")

//      val what = methodImplWhere.get.patternMatch(1).rest.toString

      val a: Seq[CodeLine] = methodAssertions(code, paramNames, paramTypes)

      val pat = patternMatchesToCode(code, c, methodImplWhere.get)

      val what2 = codeLinesToString(2, a ++ pat)

      // @todo The function is parameterized by <${ft}> but I deleted that to make it work
      Some(s"""
         |  function ${methodImplWhere.get.methodDefinition.name}($param): ${idrisTypeToTypescriptType(code, last.param.name, ft)} {
         |${what2}
         |  }""".stripMargin)
    } else None
  }

  def stringLinesToCodeLines(stripMargin: String): Seq[CodeLine] = {
    for (l <- stripMargin.split("\n").toSeq) yield defaultCodeLine(l)
  }

  def docComments(code: CodeGenerationPreferences, params: Seq[(String, String)]): Seq[CodeLine] = {
    val codeLines = (for (p <- params.zipWithIndex) yield {
      defaultCodeLine(s"* @param ${p._1._1} ${idrisTypeToTypescriptType(code, p._1._2, "a")} ?")
    }) :+ defaultCodeLine(s"* @returns ?")

    val parameterJsDoc = codeLinesToString(0, codeLines)

    stringLinesToCodeLines(s"""/**
     |* ?
     |${parameterJsDoc}
     |*/""".stripMargin)
  }


  def generateLocalVariables(methodName: String, parameterNames: Seq[String], parameterTypes: Seq[String]): Seq[LocalVariable] = {

    for (n <- parameterNames.zip(parameterTypes).zipWithIndex) yield {
      LocalVariable(methodName, n._1._1, n._1._2, n._2, None)
    }
  }

  def generateLocalVariables(methodName: String, namesAndTypes: Seq[(String, String)], parameterBinding: ParameterBinding): Seq[LocalVariable] = {
    //assert(parameterNames.size == parameterBinding.bindings.size)
    for (n <- namesAndTypes.zip(parameterBinding.bindings).zipWithIndex) yield {
      LocalVariable(methodName, n._1._1._1, n._1._1._2, n._2, n._1._2.asInstanceOf[Binding].origionalName)
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
      s"${method.methodDefinition.name}Param${p+1}"
    }

    val bindings = for (p <- parameterNames.zip(methodLineParameters).zip(methodDef)) yield {
      // @todo Bracketed binding
      Binding(p._1._1, if (p._1._2.name.isEmpty) None else if (isDataType(p._1._2.name.get)) None else Some(p._1._2.name.get), p._2.param.name, None)
    }
    ParameterBinding(bindings)
  }

  def quoteString(code: CodeGenerationPreferences, str: String) = {
    if (code.useSingleQuotesElseDoubleQuotes) s"""'${str}'""" else s""""${str}""""
  }

  def toTypescript(line: PartialCodeLine): String = {
    line.code
  }

  def toTypescript2(line: Seq[PartialCodeLine]): String = {
    (for (l <- line) yield l.code).mkString
  }

  def toTypescript3(line: Seq[CodeLine]): String = {
    (for (l <- line) yield toTypescript2(l.line)).mkString
  }

  /*
    @todo Header should be generated from the codelines as used
     */
  def defaultHeader(code: CodeGenerationPreferences) = {
    s"""// https://github.com/emmanueltouzery/prelude-ts
       |// npm install --save prelude-ts
       |import { Vector, LinkedList } from ${quoteString(code, "prelude-ts")}
       |// https://www.npmjs.com/package/offensive
       |// npm install --save offensive
       |import ${quoteString(code, "offensive/assertions/length")}
       |import ${quoteString(code, "offensive/assertions/anInteger")}
       |import ${quoteString(code, "offensive/assertions/greaterThanOrEqualTo")}
       |import check from ${quoteString(code, "offensive")}
       |// https://www.npmjs.com/package/winston
       |// npm install --save winston
       |import * as winston from 'winston'
       |
       |const myLogger = winston.createLogger({
       |    format: winston.format.json(),
       |    transports: [
       |        new winston.transports.File({filename: process.cwd() +'/project.logs'}),
       |    ],
       |
       |})
       |
       |function head${code.listType()}<a>(param: ${code.listType()}<a>): a {
       |  return param.head().getOrThrow()
       |}
       |
       |function tail${code.listType()}<a>(param: ${code.listType()}<a>): ${code.listType()}<a> {
       |  return param.tail().getOrElse(${toTypescript(emptyListInstance(code))})
       |}
       |
       |""".stripMargin
  }

  def createNodeJSProject(code: CodeGenerationPreferences) = {
    val projects = "." / "projects" / "nodejs"
    projects.createDirectoryIfNotExists()

    val readme = "." / "projects" / "nodejs" / "README.md"
    readme.writeText("README")

    val packageJson = """{
                        |  "name": "generated",
                        |  "version": "1.0.0",
                        |  "description": "README",
                        |  "main": "index.js",
                        |  "scripts": {
                        |    "test": "echo \"Error: no test specified\" && exit 1"
                        |  },
                        |  "author": "",
                        |  "license": "ISC"
                        |}
                        |
                        |""".stripMargin

    val packageJsonFile = "." / "projects" / "nodejs" / "package.json"
    packageJsonFile.writeText(packageJson)



    //val file = "." / "test.txt" //root/"tmp"/"test.txt"
    //file.overwrite("hello")
    //file.appendLine().append("world")
    //assert(file.contentAsString == "hello\nworld")
  }

  def stringToCodeLines(function: String): Seq[CodeLine] = {
    function.split("\n").map { line => CodeLine(Seq(PartialCodeLine(line))) }
  }

  def something(code: CodeGenerationPreferences,
                codeEnvironment: CodeEnvironment,
               header: String,
                test: Seq[CodeLine],
                value: Method,
                params: Seq[(String, String)],
                ft: String,
                parametersStr: Seq[String],
                parameterTypes: Seq[String]): Seq[CodeLine] = {

    // value
    //val methodImpl = methodCall(codeEnvironment, code, value.patternMatch.head.methodCall)

    // Find all method definitions
    val methodDefs = for (p <- value.patternMatch) yield methodDefinition(p.methodImplWhere, code, codeEnvironment)
    val methodDef = methodDefs.flatten.mkString("\n")

    val functionDoc = docComments(code, params)

    val parameterized = if (ft.trim.isEmpty) "" else s"<$ft>"

    val paramNames = params.map(_._1)
    val paramTypes = params.map(_._2)

    val m = methodAssertions(code, paramNames, paramTypes)

    val methodImpl = codeLinesToString(1, test)
    val methodDefAndImpl = if (methodDef.trim.isEmpty) methodImpl else
      s"""${methodDef}
         |${methodImpl}""".mkString

    val alsoAssert: String = if (m.isEmpty) methodDefAndImpl else
      s"""${codeLinesToString(1, m)}
         |$methodDefAndImpl""".stripMargin

    val function = header + s"""${toTypescript3(functionDoc)}
                      |export function ${value.methodDefinition.name}${parameterized}(${parametersStr.mkString(", ")}): ${idrisTypeToTypescriptType(code, parameterTypes.last, ft)}
                      |{
                      |${alsoAssert}
                      |}""".stripMargin

    stringToCodeLines(function)
  }

  def namesAndTypesOf(bindings: Seq[Binding], parameters: Seq[Grammar.MethodDefinitionParameter]): Seq[(String, String)] = {
    bindings.map(_.localName).zip(parameters.map(_.param.name))
  }

  /*
          val parameterNames = methodLineParameterBindings.head.bindings.map(b => {
            b.localName
          })
          val parameterTypes: Seq[String] = for (p <- method.methodDefinition.parameters) yield (p.param.name)
   */

  def toTypescriptAST(fileName: String,
                      idrisAst: Parsed[Grammar.ParsedFile],
                      code: CodeGenerationPreferences) = {

    idrisAst match {
      case Parsed.Success(parsedIdrisCode, index) => {

        if (parsedIdrisCode.method.isDefined) {
          val method = parsedIdrisCode.method.get

          val methodLineParameterBindings = for (m <- method.patternMatch)
            yield parameterBinding(method, m)

          val parameterNamesAndTypes = namesAndTypesOf(methodLineParameterBindings.head.bindings, method.methodDefinition.parameters)

          val codeEnvironment = CodeEnvironment(localVariablesFromMethodParameterScope =
              generateLocalVariables(method.methodDefinition.name, parameterNamesAndTypes, methodLineParameterBindings(0)),
            generationPreferences = code)

          val methodCall = patternMatchesToCode(code, codeEnvironment, method)

          // In the case of multiple method lines then the variable names maybe different so we need to use
          // some common name
          val parameterNamesFiltered = parameterNamesAndTypes.map(f => if (f._1.nonEmpty && f._1.head.isLower) Some(f) else None)

          val ft = functionTypeParameters(method)

          val params = parameterNamesFiltered.flatten

          val parametersStr: Seq[String] = for (p <- params) yield (p._1 + ": " + idrisTypeToTypescriptType(code, p._2, ft))

          val header = defaultHeader(code)

          //val methodChoices = patternMatchesToCodeForMainFunction(codeEnvironment, value)
          val parameterTypes = params.map(_._2)

          val codeGenerated: Seq[CodeLine] = something(code, codeEnvironment, header, methodCall, method, params, ft, parametersStr, parameterTypes)

          // @todo At this point need codelines

          val output = codeLinesToString(0, codeGenerated)

          // Generate project and files

          createNodeJSProject(code)

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







/*
  def methodCall(ce: CodeEnvironment, code: CodeGenerationPreferences, methodCall: Grammar.MethodCall): CodeLine = {
    if (methodCall.isReferenceNotMethodCall)
      defaultCodeLine(s"return ${localVariable(ce, methodCall.method.name).get}")
    else {
      val params = for (p <- methodCall.parameter) yield {
        p match {
          case i: Identifier => {
            localVariable(ce, i.name).get
          }
          case a: ArrayIdentifier => {
            if (a.isEmpty)
              emptyListInstance(code)
            else {
              throw new Exception("Error")
              // @todo Handle case
              defaultCodeLine("")
            }
          }
        }
      }
      if (isCapitalized(methodCall.method.name))
        defaultCodeLine(s"return ${idrisTypeToTypescriptType(code, methodCall.method.name, "")}")
      else
        defaultCodeLine(s"return ${methodCall.method.name}(${params.mkString(", ")})")
    }
  }



  def patternMatchesToCodeForMainFunction(codeEnvironment: CodeEnvironment, methodImplWhere: Method): Seq[CodeLine] = {
    null
  }

 */