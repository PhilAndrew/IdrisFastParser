package fold.parse.idris.typescript

object StringOps {

  def isCapitalized(name: String): Boolean = {
    name.headOption.fold(false)(_.isUpper)
  }

}
