import com.maj.ast.ASTNode
import com.maj.codegen.CodeGenerator
import com.maj.codegen.emitters.StringBufferEmitter
import com.maj.parser._
import com.maj.typecheck.BaseTypeChecker


// TODO: Pointers and references
// TODO: Structs
// TODO: Optimizations
// TODO: Block Scope checking

object Main {
  def main(args: Array[String]): Unit = {
    implicit val emitter: StringBufferEmitter = new StringBufferEmitter()
    val sourceFile = scala.io.Source.fromFile("src/main/resources/test.maj", "utf-8")
    val lines = sourceFile.getLines.mkString("\n")
    val ast: ASTNode = Token.parser.parseToCompletion(lines)
    val codeGen = new CodeGenerator()
    val typeCheck = BaseTypeChecker()
    typeCheck.visit(ast)
    codeGen.visit(ast)
    emitter.writeToFile("src/main/resources/hello.s")
    sourceFile.close()
  }
}


