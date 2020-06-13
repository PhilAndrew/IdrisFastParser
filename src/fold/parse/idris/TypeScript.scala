package fold.parse.idris

import java.nio.file.{Files, Path}

import fastparse.Parsed
import fold.parse.idris.Grammar.{ArrayIdentifier, Extraction, Identifier}

object TypeScript {

  // Use linked list
  // https://github.com/immutable-js/immutable-js

  def basicTypeToTypescript(t: String, ft: String): String = if (t == "List") s"Vector<${ft}>" else t

  def functionTypeParameters(value: Grammar.Method) = {
    s"${value.methodDefinition.rest.head.rest.head.name}"
  }

  def methodCall(methodCall: Grammar.MethodCall) = {
    val params = for (p <- methodCall.parameter) yield {
      p match {
        case i: Identifier => i.name
        case a: ArrayIdentifier => {
          if (a.isEmpty)
            "Vector.of()"
          else {
            throw new Exception("Error")
            ""
          }
        }
      }
    }
    s"  return ${methodCall.method.name}(${params.mkString(", ")})"
  }

  def buildExtractor(patternMatch: Grammar.PatternMatch) = {
    (for (r <- patternMatch.rest) yield {
      r match {
        case e: Extraction => {
          /*s"""const [${e.first.name}, ...${e.second.name}Temp] = param2.toArray()
             |const ${e.second.name} = Vector.of(...${e.second.name}Temp)
             |""".stripMargin*/

          s"""const ${e.first.name} = head(param2)
             |const ${e.second.name} = tail(param2)
             |""".stripMargin
        }
        case _ => ""
      }
    }).mkString("\n")
  }

  def methodDefinition(methodImplWhere: Grammar.MethodImplWhere) = {
    val last = methodImplWhere.methodDefinition.rest.last
    val r = methodImplWhere.methodDefinition.rest.dropRight(1)
    val paramTypes = for (p <- r) yield p.firstParam.name
    val ft = "a"
    val paramNames = paramTypes.zipWithIndex.map((t : (String, Int)) => s"param${t._2+1}")
    val param = paramTypes.zipWithIndex.map((t: (String, Int)) => s"${paramNames(t._2)}: ${basicTypeToTypescript(t._1, "a")}").mkString(", ")

    val methodBody: String = (for (p <- methodImplWhere.patterns) yield {
      p.toString
    }).mkString("\n")

    val what = methodImplWhere.patterns(1).rest.toString

    val what2 = buildExtractor(methodImplWhere.patterns(1))

    s"""  function ${methodImplWhere.methodDefinition.name}<${ft}>($param): ${basicTypeToTypescript(last.firstParam.name, ft)} {
       |${methodBody}
       |
       |${what}
       |
       |${what2}
       |
       |    return Vector.of()
       |  }
       |""".stripMargin
  }

  def toTypescriptAST(idrisAst: Parsed[Grammar.Method]) = {
    idrisAst match {
      case Parsed.Success(value, index) => {
        val parameterTypes = for (p <- value.methodDefinition.rest) yield (p.firstParam.name)
        val parameterNames = for (p <- value.methodLine.left.rest) yield (p.name)
        val ft = functionTypeParameters(value)

        val params = parameterNames.zip(parameterTypes)
        val test = for (p <- params) yield (p._1 + ": " + basicTypeToTypescript(p._2, ft))

        val header =
          """// npm install --save prelude-ts
            |import { Vector } from "prelude-ts";
            |
            |function head<a>(param: Vector<a>): a {
            |  return param.head().getOrThrow()
            |}
            |
            |function tail<a>(param: Vector<a>): Vector<a> {
            |  return param.tail().getOrElse(Vector.of())
            |}
            |
            |""".stripMargin

        val mc = methodCall(value.methodLine.methodCall)
        val methodImpl = mc

        val methodDef = methodDefinition(value.methodLine.methodImplWhere)

        // function reverse<T>(xs: Vector<T>): Vector<T>
        val function = s"""/**
                          | * ?
                          | * @param Vector<a> ?
                          | */
                          |export function ${value.methodDefinition.name}<${ft}>(${test.mkString(", ")}): ${basicTypeToTypescript(parameterTypes.last, ft)}
           |{
           |${methodDef}
           |${methodImpl}
           |}""".stripMargin

        val output = header + function

        Files.writeString(Path.of("typescript/src/test/generated.ts"), output)

        output
      }


      case _ => {
        ""
      }
    }
  }

}
