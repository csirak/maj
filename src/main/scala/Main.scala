import com.maj.ast.ASTNode
import com.maj.codegen.CodeGenerator
import com.maj.codegen.emitters.StringBufferEmitter
import com.maj.parser._
import com.maj.typecheck.TypeChecker


// TODO: Pointers and references
// TODO: Structs
// TODO: Optimizations
// TODO: ASM blocks

object Main {
  def main(args: Array[String]): Unit = {
    compile("src/main/resources/test.maj", "src/main/resources/hello")
  }

  def compile(file: String, output: String): Unit = {
    implicit val emitter: StringBufferEmitter = new StringBufferEmitter()

    val codeGen = new CodeGenerator()
    val typeCheck = new TypeChecker()

    val ast: ASTNode = Token.parser.parseFile(file)

    typeCheck.visit(ast)
    codeGen.visit(ast)

    emitter.writeToFile(s"$output.s")
  }
}


