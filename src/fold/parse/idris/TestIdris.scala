package fold.parse.idris

import java.nio.file.{Files, Path}

import fold.parse.idris.TypeScript.CodeGenerationPreferences

object TestIdris extends App {
  import fastparse._

  def testModule = {
    val str = "module test;"
    val result = parse(str, Grammar.moduleHeader(_))
    println(result)
  }
  //testModule

  def testMethod = {
    val str = "reverse : {List1 a} -> ( List2 b c) -> List3"
    val result = parse(str, Grammar.methodDecl(_))
    println(result)
  }
  //testMethod

  def testImport = {
    val str = "import identifier;"
    val result = parse(str, Grammar.importLine(_))
    println(result)
  }
  //testImport

  // Part of
  // reverse xs = revAcc [] xs where
  def testMethodCall = {
    val str = "revAcc [] xs where"
    val result = parse(str, Grammar.methodCall(_))
    println(result)
  }
  //testMethodCall

  println()
  def testRevAcc = {
    val str = """reverse : List a -> List a
                |reverse xs = revAcc [] xs where
                |  revAcc : List a -> List a -> List a
                |  revAcc acc [] = acc
                |  revAcc acc (x :: xs) = revAcc (x :: acc) xs
                |
                |""".stripMargin
//   revAcc acc [] = revAcc (x :: acc) xs
    val result = parse(str, Grammar.method(_))
    println(str)
    pprint.pprintln(result)

    val postProcess = PostProcess.postProcessParse(result)

    val code1 = CodeGenerationPreferences(usePreludeTsListForList = true, usePreludeTsVectorForList = false)
    TypeScript.toTypescriptAST("generatedList.ts", postProcess, code1);
    val code2 = CodeGenerationPreferences(usePreludeTsListForList = false, usePreludeTsVectorForList = true)
    TypeScript.toTypescriptAST("generatedVector.ts", postProcess, code2);
  }
  //testRevAcc

  println()
  def testIsSingleton = {
    val str = """isSingleton : Bool -> Type
                |isSingleton True = Nat
                |isSingleton False = List Nat
                |""".stripMargin
    val result = parse(str, Grammar.method(_))
    println(str)
    pprint.pprintln(result)
    val postProcess = PostProcess.postProcessParse(result)

    val code1 = CodeGenerationPreferences()
    TypeScript.toTypescriptAST("generatedIsSingleton.ts", postProcess, code1);

    println("Done")
  }
  //testIsSingleton


  println()
  def testEventOdd = {
    val str = """even : Nat -> Bool
                |even Z = True
                |even (S k) = odd k where
                |  odd : Nat -> Bool
                |  odd Z = False
                |  odd (S k) = even k
                |""".stripMargin
    val result = parse(str, Grammar.method(_))
    println(str)
    pprint.pprintln(result)
    val postProcess = PostProcess.postProcessParse(result)

    val code1 = CodeGenerationPreferences()
    TypeScript.toTypescriptAST("generatedEvenOdd.ts", postProcess, code1);

    println("Done")
  }
  testEventOdd







  /*
  def testIdrisFile(fileName: String) = {
    val str = Files.readString(Path.of("resources/idrisfiles/test01.idr"))
    println(str)
  }
  testIdrisFile("fileName")
*/
}
