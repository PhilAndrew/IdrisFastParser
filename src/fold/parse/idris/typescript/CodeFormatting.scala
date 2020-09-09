package fold.parse.idris.typescript

import TypeScript.{CodeLine, PartialCodeLine, defaultCodeLine, toTypescript2}

object CodeFormatting {

  def increaseIndentOfCodeByOneLevel(code: Seq[CodeLine]): Seq[CodeLine] = {
    for (c <- code) yield c.copy(indentLevel = c.indentLevel + 1)
  }

  def codeLinesToString(indent: Int, lines: Seq[CodeLine]): String = {
    val i = if (lines.size == 1) indent - 1 else indent
    (for (l <- lines) yield {
      "  ".repeat(i + l.indentLevel).mkString + toTypescript2(l.line)
    }).mkString("\n")
  }

  def stringLinesToCodeLines(stripMargin: String): Seq[CodeLine] = {
    for (l <- stripMargin.split("\n").toSeq) yield defaultCodeLine(l)
  }

  def stringToCodeLines(function: String): Seq[CodeLine] = {
    function.split("\n").map { line => CodeLine(Seq(PartialCodeLine(line))) }
  }

  def quoteString(code: Preferences.CodeGenerationPreferences, str: String) = {
    if (code.useSingleQuotesElseDoubleQuotes) s"""'${str}'""" else s""""${str}""""
  }

}
