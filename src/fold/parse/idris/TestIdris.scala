package fold.parse.idris

import java.nio.file.{Files, Path}

import fold.parse.idris.parse.{Grammar, PostProcess}
import fold.parse.idris.typescript.Preferences.CodeGenerationPreferences
import fold.parse.idris.typescript.TypeScript

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
    val result = parse(str, Grammar.methodImplRightSideMethodCall(_))
    println(result)
  }
  //testMethodCall

  def genTest(str: String, file: String) = {
    val result = parse(str, Grammar.method(_))
    println(str)
    pprint.pprintln(result)
    val postProcess = PostProcess.postProcessParse(result)

    val code1 = CodeGenerationPreferences()
    TypeScript.toTypescriptAST(file, postProcess, code1);

    println("Done")
  }


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
  testRevAcc

  println()

  genTest("""isSingleton : Bool -> Type
            |isSingleton True = Nat
            |isSingleton False = List Nat
            |""".stripMargin, "generatedIsSingleton.ts")

  genTest("""plus : Nat -> Nat -> Nat
            |plus Z     y = y
            |plus (S k) y = S (plus k y)""".stripMargin, "generatedPlus.ts")

  val reverseSentence =
    """reverse is List a to List a
      |[] is []
      |x :: xs is xs ++ [x]
      |""".stripMargin

  def sentenceToIdris(reverseSentence: String) = {
    ""
  }

  val reverseIdris = sentenceToIdris(reverseSentence)

  genTest("""reverse1 : List a -> List a
            |reverse1 [] = []
            |reverse1 (x :: xs) = reverse1 (xs ++ [x])""".stripMargin, "generatedReverse.ts")
/*
  genTest("""even : Nat -> Bool
            |even Z = True
            |even (S k) = odd k where
            |  odd : Nat -> Bool
            |  odd Z = False
            |  odd (S k) = even k""".stripMargin, "generatedEvenOdd.ts")
*/













  println()
  def testData() = {
    val str = """
                |data Pair a b = MkPair a b
                |""".stripMargin

    /*

fred : (String, Int)
fred = ("Fred", 42)

jim : (String, Int, String)
jim = ("Jim", 25, "Cambridge")
     */
    val result = parse(str, Grammar.fileContents(_))
    println(str)
    pprint.pprintln(result)
    val postProcess = PostProcess.postProcessParse(result)

    val code1 = CodeGenerationPreferences()
    TypeScript.toTypescriptAST("generatedDataPair.ts", postProcess, code1);


    val fakeOutput =
      """
        |class Pair<A,B> {
        |  a: A;
        |  b: B;
        |  constructor(a: A, b: B) {
        |    this.a = a;
        |    this.b = b;
        |  }
        |}
        |
        |function newMkPair<A, B>(a: A, b: B) {
        |  return new Pair<A, B>(a, b);
        |}
        |
        |
        |""".stripMargin
    val fileName = "generatedDataPair.ts"
    Files.writeString(Path.of("typescript/src/test/" + fileName), fakeOutput)




    println("Done")
  }
  //testData()

  def testData2() = {
    // Polymorphic lists
    val str =
      """
        |data List a = Nil | (::) a (List a)
        |""".stripMargin

    val fakeOutout =
      """
        |
        |interface List {
        |}
        |
        |class ListNil implements List {
        |}
        |
        |class ListAppend<A> implements List {
        |  a: A;
        |  b: List;
        |  constructor(a: A, b: List) {
        |    this.a = a;
        |    this.b = b;
        |  }
        |}
        |
        |function newListNil() {
        |  return new ListNil();
        |}
        |
        |function newListAppend<A>(a: A, b: List) {
        |  return new ListAppend<A>(a, b);
        |}
        |""".stripMargin

    val fileName = "generatedDataList.ts"
    Files.writeString(Path.of("typescript/src/test/" + fileName), fakeOutout)
  }
  //testData2()

  def testData3() = {
    // Polymorphic lists
    val str =
      """
        |data Vect : Nat -> Type -> Type where
        |  Nil  : Vect Z a
        |  (::) : (x : a) -> (xs : Vect n a) -> Vect (S n) a
        |""".stripMargin

    val fakeOutout =
      """
        |
        |interface Vect {
        |  a: number;
        |}
        |
        |class VectNil implements Vect {
        |  a: number = 0;
        |}
        |
        |class VectAppend implements Vect {
        |  a: number = 0;
        |  constructor(a: number) {
        |    this.a = a;
        |  }
        |}
        |
        |function newVectNil() {
        |  return new VectNil();
        |}
        |
        |function newVectAppend(a: number) {
        |  return new VectAppend(a);
        |}
        |
        |
        |""".stripMargin

    val fileName = "generatedDataVect.ts"
    Files.writeString(Path.of("typescript/src/test/" + fileName), fakeOutout)
  }
  //testData3()

  println()




  /*
  def testIdrisFile(fileName: String) = {
    val str = Files.readString(Path.of("resources/idrisfiles/test01.idr"))
    println(str)
  }
  testIdrisFile("fileName")
*/
}
