import com.maj.ast.AsmEmitter
import com.maj.parser._

object Main {
  def main(args: Array[String]): Unit = {
    val sourceFile = scala.io.Source.fromFile("src/main/resources/test.maj", "utf-8")
    implicit val emitter: AsmEmitter = new AsmEmitter()
    val lines = sourceFile.getLines.mkString("\n")
    
    val source = new Source(lines, 0, true)

    val ast = Token.parser.parse(source).get.value
    println(ast)
    ast.emit
    sourceFile.close()

    emitter.getOutput("src/main/resources/hello.s")
  }
}


