package fold.parse.idris.typescript

import fold.parse.idris.typescript.TypeScript.{CodeLine, defaultCodeLine, nodeJsLibraryOf, partialCodeLine, toTypescript}

object Templates {

  /*
    @todo Header should be generated from the codelines as used
     */
  def defaultHeader(code: Preferences.CodeGenerationPreferences, insertImportStatements: Seq[CodeLine]) = {

    // @todo Go through the insertImportStatements to work out what import statements to use

    val nodeJsLibraries = (for (z <- insertImportStatements) yield z.nodeJsLibraryUsage).flatten.toSet

    val d = for (s <- nodeJsLibraries; i <- s.imports) yield {
      i
    }

    val insertLib = d.mkString("\n")

    // import { Vector, LinkedList } from ${CodeFormatting.quoteString(code, "prelude-ts")}
    println(nodeJsLibraries)
    val importStatements = s"""// https://github.com/emmanueltouzery/prelude-ts
                             |// npm install --save prelude-ts
                             |$insertLib
                             |// https://www.npmjs.com/package/offensive
                             |// npm install --save offensive
                             |import ${CodeFormatting.quoteString(code, "offensive/assertions/length")}
                             |import ${CodeFormatting.quoteString(code, "offensive/assertions/anInteger")}
                             |import ${CodeFormatting.quoteString(code, "offensive/assertions/greaterThanOrEqualTo")}
                             |import check from ${CodeFormatting.quoteString(code, "offensive")}
                             |// https://www.npmjs.com/package/winston
                             |// npm install --save winston
                             |import * as winston from 'winston'""".stripMargin

    val str = TypeScript.toTypescript4(insertImportStatements)

    s"""|$importStatements
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
       |  return param.tail().getOrElse(${toTypescript(DefaultTypes.emptyListInstance(code))})
       |}
       |
       |""".stripMargin + str
  }

  def codeLineAssert(code: Preferences.CodeGenerationPreferences, c: String): CodeLine = {
    CodeLine(partialCodeLine(c), 0, nodeJsLibraryOf(code))
  }

  def methodAssertions(code: Preferences.CodeGenerationPreferences, paramNames: Seq[String], paramTypes: Seq[String]): Seq[CodeLine] = {
    val result1 = for (p <- paramNames.zip(paramTypes)) yield {
      p._2 match {
        case "Nat" => {
          Seq(codeLineAssert(code, s"""check(${p._1}, ${CodeFormatting.quoteString(code, p._1)}).is.anInteger()"""),
            codeLineAssert(code, s"""check(${p._1}, ${CodeFormatting.quoteString(code, p._1)}).is.greaterThanOrEqualTo(0)()"""))
        }
        case _ => Seq.empty
      }
    }
    val result2 = result1.flatten.toSeq
    if (result2.size > 0) result2 ++ Seq(defaultCodeLine("")) else result2
  }


}
