package fold.parse.idris

object TestIdris extends App {
  import fastparse._

  def testModule = {
    val str = "module test;"
    val result = parse(str, Grammar.moduleHeader(_))
    println(result)
  }

  def testMethod = {
    val str = "reverse : {List1 a} -> ( List2 b c) -> List3"
    val result = parse(str, Grammar.methodDecl(_))
    println(result)
  }

  testMethod
}
