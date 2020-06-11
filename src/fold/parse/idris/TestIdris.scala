package fold.parse.idris

import java.nio.file.{Files, Path}

object TestIdris extends App {
  import fastparse._

  def testModule = {
    val str = "module test;"
    val result = parse(str, Grammar.moduleHeader(_))
    println(result)
  }
  testModule

  def testMethod = {
    val str = "reverse : {List1 a} -> ( List2 b c) -> List3"
    val result = parse(str, Grammar.methodDecl(_))
    println(result)
  }
  testMethod

  def testImport = {
    val str = "import identifier;"
    val result = parse(str, Grammar.importLine(_))
    println(result)
  }
  testImport

  def testMethodCall = {
    val str = "revAcc [] xs where"
    val result = parse(str, Grammar.methodCall(_))
    println(result)
  }
  testMethodCall

  /*
  def testIdrisFile(fileName: String) = {
    val str = Files.readString(Path.of("resources/idrisfiles/test01.idr"))
    println(str)
  }
  testIdrisFile("fileName")
*/
}
