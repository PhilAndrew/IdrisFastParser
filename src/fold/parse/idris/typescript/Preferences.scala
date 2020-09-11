package fold.parse.idris.typescript

import fold.parse.idris.typescript.TypeScript.{preludeTsListForList, preludeTsVectorForList}

object Preferences {
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
    def listNodeJs(): TypeScript.NodeJsLibrary = {
      if (usePreludeTsVectorForList)
        preludeTsVectorForList
      else
        preludeTsListForList
    }

    def listType() = if (usePreludeTsVectorForList) "Vector" else if (usePreludeTsListForList) "LinkedList" else ""
  }
}
