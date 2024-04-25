import com.maj.ast.ASTNode
import com.maj.codegen.CodeGenerator
import com.maj.emitters.StringBufferEmitter
import com.maj.parser._
import com.maj.typecheck.TypeChecker


// TODO: Optimizations IR
// TODO: Structs
// TODO: Pointers & references
// TODO: Error handling
// TODO: Standard library embedded but not done
// TODO: Debug information
//    TODO: Line numbers
//    TODO: Predictive Path Analysis
// TODO: Imports & Packages


object Main {
  def main(args: Array[String]): Unit = {
    compile("src/main/resources/test.maj", "src/main/resources/hello")
  }

  def compile(file: String, output: String): Unit = {
    implicit val emitter: StringBufferEmitter = new StringBufferEmitter()
    val codeGen = new CodeGenerator()
    val typeCheck = new TypeChecker()
    val ast: ASTNode = Token.parser.parseWithStdLib(file)
    typeCheck.visit(ast)
    codeGen.visit(ast)
    emitter.writeToFile(s"$output.s")
  }
}


