package fold.parse.idris.typescript

import java.nio.file.{Files, Path}

import fastparse.Parsed
import fold.parse.idris.parse.Grammar._
import fold.parse.idris.parse.Grammar

object TypeScript {

  case class BracketedBinding(origional: Seq[String], // Assume ["S", "k"] which is (S k)
                              name: String, // Assume "k" as any place this name is found we are referring to this backeted binding
                              substitution: String // Assume "k-1" to substitute into this place
                             )

  case class Binding(localName: String, origionalName: Option[String], typeOf: String, bracketedBinding: Option[BracketedBinding]) // @todo bracketedBinding set to None
  case class ParameterBinding(bindings: Seq[Binding])


  case class LocalVariable(methodName: String, variableName: String, typeOf: String, indexInMethod: Int, variableAlias: Option[String])

  case class CodeEnvironment(scopedVariablePrefix: Option[String] = None,
                             localVariablesFromMethodParameterScope: Seq[LocalVariable] = Seq.empty,
                             generationPreferences: Preferences.CodeGenerationPreferences = Preferences.CodeGenerationPreferences())

  // For example
  // winston
  // https://www.npmjs.com/package/winston
  // npm install --save winston
  // import * as winston from 'winston'
  case class NodeJsLibrary(name: String, url: String, npmInstall: String, imports: Seq[String])

  case class PartialCodeLine(code: String, nodeJsLibraryUsage: Seq[NodeJsLibrary] = Seq.empty)

  case class CodeLine(line: Seq[PartialCodeLine], indentLevel: Int = 0, nodeJsLibraryUsage: Seq[NodeJsLibrary] = Seq.empty)

  val preludeTsVectorForList = NodeJsLibrary("Vector", "https://github.com/emmanueltouzery/prelude-ts", "prelude-ts", Seq("import { Vector } from 'prelude-ts'"))
  val preludeTsListForList = NodeJsLibrary("LinkedList", "https://github.com/emmanueltouzery/prelude-ts", "prelude-ts", Seq("import { LinkedList } from 'prelude-ts'"))
  val assertNodeJsLibrary = NodeJsLibrary("Offensive", "https://www.npmjs.com/package/offensive", "offensive",
    Seq("import 'offensive/assertions/length'",
      "import 'offensive/assertions/anInteger'",
      "import 'offensive/assertions/greaterThanOrEqualTo'",
      "import check from 'offensive'"))

  // Use linked list
  // https://github.com/immutable-js/immutable-js

  def defaultCodeLine(line: String): CodeLine = {
    CodeLine(Seq(PartialCodeLine(line)))
  }

  def nodeJsLibraryOf(preferences: Preferences.CodeGenerationPreferences): Seq[NodeJsLibrary] = {
    if (preferences.usePreludeTsListForList)
      Seq(preludeTsListForList)
    else
      Seq(preludeTsVectorForList)
  }

  def partialCodeLine(str: String): Seq[PartialCodeLine] = {
    Seq(PartialCodeLine(str))
  }

  def buildExtractor(preferences: Preferences.CodeGenerationPreferences, codeEnvironment: CodeEnvironment, patternMatch: Grammar.MethodLine): Seq[CodeLine] = {
    (for (r <- patternMatch.left.rest.zipWithIndex) yield {
      if (r._1.extractionForm.isDefined) {
        val e = r._1.extractionForm.get

        Seq(
          CodeLine(partialCodeLine(s"const ${prefix(codeEnvironment, e.first.name)} = head${codeEnvironment.generationPreferences.listType()}(${patternMatch.left.methodName.name}Param${r._2 + 1})"), 0, nodeJsLibraryOf(preferences)),
          CodeLine(partialCodeLine(s"const ${prefix(codeEnvironment, e.second.name)} = tail${codeEnvironment.generationPreferences.listType()}(${patternMatch.left.methodName.name}Param${r._2 + 1})"), 0, nodeJsLibraryOf(preferences)))
      } else if (r._1.bracketedForm.isDefined) {
        val b: Bracketed = r._1.bracketedForm.get
        if (b.first.name == "S") { // Assume (S k)
          Seq(defaultCodeLine(s"const k = ${patternMatch.left.methodName.name}Param${r._2 + 1} - 1"))
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

  def emptyCodeLine() = defaultCodeLine("")

  def partialCodeLineMkString(conditions: Seq[PartialCodeLine], start: String, join: String, end: String): Seq[PartialCodeLine] = {
    if (conditions.size == 1) {
      PartialCodeLine(start) +: conditions :+ PartialCodeLine(end)
    } else {
      val param2: Seq[PartialCodeLine] = conditions.foldLeft(Seq(PartialCodeLine(start)))(
        (a: Seq[PartialCodeLine], b: PartialCodeLine) => {
          a ++ Seq(PartialCodeLine(join)) ++ Seq(b)
        })
      PartialCodeLine(start) +: param2 :+ PartialCodeLine(end)
    }
  }

  def buildPatternMatchCondition(code: Preferences.CodeGenerationPreferences,
                                 codeEnvironment: CodeEnvironment,
                                 patternMatch: Grammar.MethodLine): Seq[CodeLine] = {

    val lines: Seq[Seq[PartialCodeLine]] = for (rr <- patternMatch.left.rest.zipWithIndex) yield {
      val r = rr._1.name
      val x: Seq[PartialCodeLine] = if (r.isDefined) {
        val param = s"${patternMatch.left.methodName.name}Param${rr._2 + 1}"
        if (r.get == "[]")
          Seq(PartialCodeLine(s"${param}.isEmpty()"))
        else if (r.get == "Z")
          Seq(integerComparison(code, param, 0))
        else if (r.get == "True")
          Seq(PartialCodeLine(s"${param} === true"))
        else if (r.get == "False")
          Seq(PartialCodeLine(s"${param} === false"))
        else
          Seq.empty
      } else Seq.empty
      x
    }

    val conditions: Seq[PartialCodeLine] = lines.flatten
    if (conditions.isEmpty) Seq.empty //Seq(CodeLine("{"))
    else Seq(CodeLine(Seq(PartialCodeLine("if ")) ++ {
      partialCodeLineMkString(conditions, "(", " && ", ") {")
    }))


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

  def integerComparison(code: Preferences.CodeGenerationPreferences, param: String, compareToken: Int): PartialCodeLine = {
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

  def createEmptyArray(code: Preferences.CodeGenerationPreferences): String = {
    s"${code.listType()}.of()"
  }

  def emptyArrayFixThis(code: Preferences.CodeGenerationPreferences, name: String): String = {
    if (name == "[]") createEmptyArray(code) else s""""$name""""
  }

  def buildCode(code: Preferences.CodeGenerationPreferences, codeEnvironment: CodeEnvironment, patternMatch: Grammar.MethodLine): Seq[CodeLine] = {

    val c2 = updateCodeEnvironment(codeEnvironment, patternMatch)

    def la(i: Identifier) = {
      if (StringOps.isCapitalized(i.name)) {
        i.name
      } else {
        localVariable(c2, declaredIdentifier(c2, i.name)).getOrElse(i.name)
      }
    }

    if (patternMatch.statement.dataValue.isDefined) {
      val r = (for (i <- patternMatch.statement.dataValue.get.rest) yield i.name).mkString(" ")
      val i = if (r.size > 0) " " + r else ""

      val c = DefaultTypes.constantOf(patternMatch.statement.dataValue.get.name.name)
      if (c.isDefined)
        Seq(defaultCodeLine(s"""return ${c.get}"""))
      else
        Seq(defaultCodeLine(s"""return ${emptyArrayFixThis(code, patternMatch.statement.dataValue.get.name.name)}$i"""))
    } else if ((patternMatch.statement.methodCall.isDefined) && (patternMatch.statement.methodCall.get.isReferenceNotMethodCall))
      Seq(defaultCodeLine(s"return ${localVariable(c2, patternMatch.statement.methodCall.get.method.name).get}"))
    else {
      val p = for (p <- patternMatch.statement.methodCall.get.parameter) yield {
        p match {
          case i: Identifier => {
            la(i)
          }
          case e: MethodExpression => {
            // Do prepend
            e.expressionType match {
              case ex: HeadTailExpression => s"${localVariable(c2, e.second.name).get}.prepend(${e.first.name})"
              case ex: ListAppendExpression => s"${e.first.name}.append(${localVariable(c2, e.second.name).get})"
              case _ => ""
                // xs.append(x)
            }
          }
          case a: ArrayIdentifier => {
            if (a.isEmpty)
              toTypescript(DefaultTypes.emptyListInstance(code))
            else {
              throw new Exception("Error")
              ""
            }
          }
          case _ => ""
        }
      }

      val parameters = p.mkString(", ")
      if (StringOps.isCapitalized(patternMatch.statement.methodCall.get.method.name))
        Seq(CodeLine(Seq(PartialCodeLine("return "),
          TypeConversion.idrisTypeToTypescriptType(code, patternMatch.statement.methodCall.get.method.name, ")"))))
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
        val prefix = s"${patternMatch.left.methodName.name}Param${p._2 + 1}"
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

    val flat: Seq[LocalVariable] = found.flatten.flatten

    codeEnvironmentNested.copy(localVariablesFromMethodParameterScope = flat ++ codeEnvironmentNested.localVariablesFromMethodParameterScope)
  }

  // @todo Merge the next two functions
  def patternMatchesToCode(code: Preferences.CodeGenerationPreferences, codeEnvironment: CodeEnvironment, method: Grammar.Method): Seq[CodeLine] = {

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
        defaultCodeLine(s"// Pattern matching function ${m._2 + 1}") +: joined
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

  def methodDefinition(methodImplWhere: Option[Grammar.Method], code: Preferences.CodeGenerationPreferences, c: CodeEnvironment): Option[Seq[CodeLine]] = {
    if (methodImplWhere.isDefined) {
      val last = methodImplWhere.get.methodDefinition.parameters.last
      val r = methodImplWhere.get.methodDefinition.parameters.dropRight(1)
      val paramTypes = for (p <- r) yield p.param.name
      val ft = "a"
      val paramNames = paramTypes.zipWithIndex.map((t: (String, Int)) => s"${methodImplWhere.get.methodDefinition.name}Param${t._2 + 1}")
      val param1: Seq[Seq[PartialCodeLine]] = paramTypes.zipWithIndex.map((t: (String, Int)) => Seq(PartialCodeLine(s"${paramNames(t._2)}: "),
        TypeConversion.idrisTypeToTypescriptType(code, t._1, ft)))

      val param2: Seq[PartialCodeLine] = param1.foldLeft(Seq[PartialCodeLine]())(
        (a: Seq[PartialCodeLine], b: Seq[PartialCodeLine]) => {
          if (a.isEmpty) b else
            a ++ Seq(PartialCodeLine(", ")) ++ b
        })

      val methodBody: String = (for (p <- methodImplWhere.get.patternMatch) yield {
        p.toString
      }).mkString("\n")

      //      val what = methodImplWhere.get.patternMatch(1).rest.toString

      val a: Seq[CodeLine] = Templates.methodAssertions(code, paramNames, paramTypes)

      val pat = patternMatchesToCode(code, c, methodImplWhere.get)

      //val what2 = codeLinesToString(2, a ++ pat)

      val m = CodeFormatting.increaseIndentOfCodeByOneLevel(a ++ pat)

      // @todo The function is parameterized by <${ft}> but I deleted that to make it work
      val result: Seq[CodeLine] = Seq(
        CodeLine(Seq(PartialCodeLine(s"function ${methodImplWhere.get.methodDefinition.name}(")) ++
          param2 ++
          Seq(PartialCodeLine("): "),
            TypeConversion.idrisTypeToTypescriptType(code, last.param.name, ft), PartialCodeLine(" {")))) ++
        m ++ Seq(defaultCodeLine("}"))

      Some(result)
    } else None
  }

  def docComments(code: Preferences.CodeGenerationPreferences, params: Seq[(String, String)]): Seq[CodeLine] = {
    val codeLines = (for (p <- params.zipWithIndex) yield {
      defaultCodeLine(s"* @param ${p._1._1} ${toTypescript(TypeConversion.idrisTypeToTypescriptType(code, p._1._2, "a"))} ?")
    }) :+ defaultCodeLine(s"* @returns ?")

    val parameterJsDoc = CodeFormatting.codeLinesToString(0, codeLines)

    CodeFormatting.stringLinesToCodeLines(
      s"""/**
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



  def toTypescript(line: PartialCodeLine): String = {
    line.code
  }

  def toTypescript2(line: Seq[PartialCodeLine]): String = {
    (for (l <- line) yield l.code).mkString
  }

  def toTypescript3(line: Seq[CodeLine]): String = {
    (for (l <- line) yield toTypescript2(l.line)).mkString
  }

  def toTypescript4(line: Seq[CodeLine]): String = {
    (for (l <- line) yield toTypescript2(l.line)).mkString("\n")
  }

  def toCodeLines(str: String): Seq[CodeLine] = {
    for (l <- str.split("\n")) yield CodeLine(Seq(PartialCodeLine(l, Seq.empty)), 0, Seq.empty)
  }

  /*
  def createNodeJSProject(code: Preferences.CodeGenerationPreferences) = {
    val projects = "." / "projects" / "nodejs"
    projects.createDirectoryIfNotExists()

    val readme = "." / "projects" / "nodejs" / "README.md"
    readme.writeText("README")

    val package
      son =
        """{

        ted",

        0.0",

        DME",

        .js",

        s": {
                        |    "test": "echo \"Error:
        it 1"
        |  },

        : "",

        "I

            |
                        |""".stripMargin

    val packageJsonFile = "." / "projects" / "nodejs" / "package.json"
    packageJsonFile.writeText(packageJson)



    //val file = "." / "test.txt" //root/"tmp"/"test.txt"
    //file.overwrite("hello")
    //file.appendLine().append("world")
    //assert(file.contentA


   == "hello\nworld")
  }*/




  def something(code: Preferences.CodeGenerationPreferences,
                codeEnvironment: CodeEnvironment,
                header: (Seq[CodeLine]) => Seq[CodeLine],
                test: Seq[CodeLine],
                value: Method,
                params: Seq[(String, String)],
                ft: String,
                parametersStr: Seq[PartialCodeLine],
                parameterTypes: Seq[String]): Seq[CodeLine] = {

    // value
    //val methodImpl = methodCall(codeEnvironment, code, value.patternMatch.head.methodCall)

    // Find all method definitions
    val methodDefs1: Seq[Option[Seq[CodeLine]]] = (for (p <- value.patternMatch) yield methodDefinition(p.methodImplWhere, code, codeEnvironment))

    val methodDefs: Seq[CodeLine] = methodDefs1.flatten.flatten
    //val methodDef = methodDefs.flatten.mkString("\n")

    val functionDoc = docComments(code, params)

    val parameterized = if (ft.trim.isEmpty) "" else s"<$ft>"

    val paramNames = params.map(_._1)
    val paramTypes = params.map(_._2)

    val m = Templates.methodAssertions(code, paramNames, paramTypes)

    //val methodImpl = codeLinesToString(1, test)
    val methodDefAndImpl = if (methodDefs.isEmpty) test else methodDefs ++ test

    val alsoAssert: Seq[CodeLine] = if (m.isEmpty) {
      methodDefAndImpl
    } else {
      m ++ methodDefAndImpl
    }


    val indented = CodeFormatting.increaseIndentOfCodeByOneLevel(alsoAssert)
    //val headerCodeLines = CodeFormatting.stringToCodeLines(header)
    // functionDoc
    val functionCodeLines: Seq[CodeLine] = Seq(
      CodeLine(
        Seq(PartialCodeLine(s"export function ${value.methodDefinition.name}${parameterized}(")) ++ parametersStr ++ Seq(
          PartialCodeLine("): ")) ++
            Seq(TypeConversion.idrisTypeToTypescriptType(code, parameterTypes.last, ft))),
    defaultCodeLine("{")) ++
      indented ++ Seq(defaultCodeLine("}"))

    val codeBefore = functionDoc ++ functionCodeLines
    val codeAfter = insertImportStatements(codeBefore, header)
    val function: Seq[CodeLine] = codeAfter
    function
  }

  def functionParametersBindedWithNames(bindings: Seq[Binding], parameters: Seq[Grammar.MethodDefinitionParameter]): Seq[(String, String)] = {
    bindings.map(_.localName).zip(parameters.map(_.param.name))
  }

  /*
          val parameterNames = methodLineParameterBindings.head.bindings.map(b => {
            b.localName
          })
          val parameterTypes: Seq[String] = for (p <- method.methodDefinition.parameters) yield (p.param.name)
   */

  def insertImportStatements(codeGenerated: Seq[CodeLine], header: (Seq[CodeLine]) => Seq[CodeLine]): Seq[CodeLine] = {
    header(codeGenerated)
  }

  def toTypescriptAST(fileName: String,
                      idrisAst: Parsed[Grammar.ParsedFile],
                      code: Preferences.CodeGenerationPreferences) = {

    idrisAst match {
      case Parsed.Success(parsedIdrisCode, index) => {

        if (parsedIdrisCode.method.isDefined) {
          val method = parsedIdrisCode.method.get

          val methodLineParameterBindings = for (m <- method.patternMatch)
            yield parameterBinding(method, m)

          val parameterNamesAndTypes = functionParametersBindedWithNames(methodLineParameterBindings.head.bindings, method.methodDefinition.parameters)

          val codeEnvironment = CodeEnvironment(
            localVariablesFromMethodParameterScope =
              generateLocalVariables(method.methodDefinition.name, parameterNamesAndTypes, methodLineParameterBindings(0)),
            generationPreferences = code)

          val methodCall = patternMatchesToCode(code, codeEnvironment, method)

          // In the case of multiple method lines then the variable names maybe different so we need to use
          // some common name
          val parameterNamesFiltered = parameterNamesAndTypes.map(f => if (f._1.nonEmpty && f._1.head.isLower) Some(f) else None)

          val ft = functionTypeParameters(method)

          val params = parameterNamesFiltered.flatten

          val parametersStr: Seq[PartialCodeLine] = (for (p <- params) yield (Seq(PartialCodeLine(p._1 + ": "), TypeConversion.idrisTypeToTypescriptType(code, p._2, ft)))).flatten

          val header = (s: Seq[CodeLine]) => Templates.defaultHeader(code, s)

          //val methodChoices = patternMatchesToCodeForMainFunction(codeEnvironment, value)
          val parameterTypes = method.methodDefinition.parameters.map(_.param.name)

          val codeGenerated: Seq[CodeLine] = something(code, codeEnvironment, header, methodCall, method, params, ft, parametersStr, parameterTypes)

          val codeWithImportStatements: Seq[CodeLine] = codeGenerated

          val output = CodeFormatting.codeLinesToString(0, codeWithImportStatements)

          // Generate project and files

          //createNodeJSProject(code)

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
