package fold.parse.idris.typescript

import fold.parse.idris.typescript.TypeScript.PartialCodeLine

object TypeConversion {

  /**
   * Converts an Idris type to an equivalent Typescript type
   *
   * @param preferences
   * @param idrisType
   * @param parameterizedType
   * @return
   */
  def idrisTypeToTypescriptType(preferences: Preferences.CodeGenerationPreferences,
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
}
