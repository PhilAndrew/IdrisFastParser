package fold.parse.idris.typescript

import fold.parse.idris.typescript.TypeScript.{CodeLine, NodeJsLibrary, defaultCodeLine, nodeJsLibraryOf, partialCodeLine, toTypescript}

object Templates {

  /*
    @todo Header should be generated from the codelines as used
     */
  def defaultHeader(code: Preferences.CodeGenerationPreferences, insertImportStatements: Seq[CodeLine]): Seq[CodeLine] = {

    // @todo Go through the insertImportStatements to work out what import statements to use

    val nodeJsLibraries = (for (z <- insertImportStatements;
                                k <- z.line) yield z.nodeJsLibraryUsage ++ k.nodeJsLibraryUsage).flatten.toSet

    val d = for (s <- nodeJsLibraries) yield {
      "// " + s.name + "\n" + "// " + s.url + "\n" +
      "// npm install --save " + s.npmInstall + "\n" + s.imports.mkString("\n")
    }

    val insertLib = d.mkString("\n")

    // import { Vector, LinkedList } from ${CodeFormatting.quoteString(code, "prelude-ts")}
    println(nodeJsLibraries)
    val importStatements = s"""// Calculated import statements
                             |$insertLib""".stripMargin

    /*
    // https://www.npmjs.com/package/winston
// npm install --save winston
import * as winston from 'winston'


const myLogger = winston.createLogger({
    format: winston.format.json(),
    transports: [
        new winston.transports.File({filename: process.cwd() +'/project.logs'}),
    ],

})

     */

    //val str = TypeScript.toTypescript4(insertImportStatements)

    val headerInsert = s"""|$importStatements
       |
       |function head${code.listType()}<a>(param: ${code.listType()}<a>): a {
       |  return param.head().getOrThrow()
       |}
       |
       |function tail${code.listType()}<a>(param: ${code.listType()}<a>): ${code.listType()}<a> {
       |  return param.tail().getOrElse(${toTypescript(DefaultTypes.emptyListInstance(code))})
       |}
       |
       |""".stripMargin

    TypeScript.toCodeLines(headerInsert) ++ insertImportStatements
  }

  def codeLineAssert(code: Preferences.CodeGenerationPreferences, c: String, nodeJsLibrary: Seq[NodeJsLibrary]): CodeLine = {
    CodeLine(partialCodeLine(c), 0, nodeJsLibrary)
  }

  def methodAssertions(code: Preferences.CodeGenerationPreferences, paramNames: Seq[String], paramTypes: Seq[String]): Seq[CodeLine] = {
    val result1 = for (p <- paramNames.zip(paramTypes)) yield {
      p._2 match {
        case "Nat" => {
          Seq(codeLineAssert(code, s"""check(${p._1}, ${CodeFormatting.quoteString(code, p._1)}).is.anInteger()""", Seq(TypeScript.assertNodeJsLibrary)),
            codeLineAssert(code, s"""check(${p._1}, ${CodeFormatting.quoteString(code, p._1)}).is.greaterThanOrEqualTo(0)()""", Seq(TypeScript.assertNodeJsLibrary)))
        }
        case _ => Seq.empty
      }
    }
    val result2 = result1.flatten.toSeq
    if (result2.size > 0) result2 ++ Seq(defaultCodeLine("")) else result2
  }


}
