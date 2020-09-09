package fold.parse.idris.typescript

import fold.parse.idris.typescript.TypeScript.{PartialCodeLine, preludeTsListForList, preludeTsVectorForList}

object DefaultTypes {
  val OF = "of()"

  def constantOf(name: String): Option[String] = {
    if (name.equals("True"))
      Some("true")
    else if (name.equals("False"))
      Some("false")
    else None
  }

  def of(str: String): String = {
    s"$str.$OF"
  }

  def emptyListInstance(preferences: Preferences.CodeGenerationPreferences): PartialCodeLine = {
    if (preferences.usePreludeTs) {
      if (preferences.usePreludeTsVectorForList)
        PartialCodeLine(of("Vector"), Seq(preludeTsVectorForList))
      else if (preferences.usePreludeTsListForList)
        PartialCodeLine(of("LinkedList"), Seq(preludeTsListForList))
      else // @todo Handle this case
        PartialCodeLine(of("LinkedList"), Seq(preludeTsListForList))
    } else {
      // @todo Handle this case
      PartialCodeLine(of("LinkedList"), Seq(preludeTsListForList))
    }
  }
}
