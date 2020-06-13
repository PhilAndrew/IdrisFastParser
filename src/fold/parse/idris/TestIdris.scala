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

  // Part of
  // reverse xs = revAcc [] xs where
  def testMethodCall = {
    val str = "revAcc [] xs where"
    val result = parse(str, Grammar.methodCall(_))
    println(result)
  }
  testMethodCall

  println()
  def testAll = {
    val str = """reverse : List a -> List a
                |reverse xs = revAcc [] xs where
                |  revAcc : List a -> List a -> List a
                |  revAcc acc [] = revAcc (x :: acc) xs
                |  revAcc acc (x :: xs) = revAcc acc xs
                |
                |""".stripMargin
    // revAcc (x :: acc) xs
    val result = parse(str, Grammar.method(_))
    println(str)
    pprint.pprintln(result)
  }
  testAll

  /*
reverse : List a -> List a
reverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs
   */



  /*
  def testIdrisFile(fileName: String) = {
    val str = Files.readString(Path.of("resources/idrisfiles/test01.idr"))
    println(str)
  }
  testIdrisFile("fileName")
*/
}
