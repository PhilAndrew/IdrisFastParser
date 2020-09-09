package fold.examples

import better.files._

object BetterFilesExample extends App {

  //val f = File("/User/johndoe/Documents")                      // using constructor

  val file = "." / "test.txt" //root/"tmp"/"test.txt"
  file.overwrite("hello")
  file.appendLine().append("world")
  assert(file.contentAsString == "hello\nworld")

}
