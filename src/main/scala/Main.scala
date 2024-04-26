import com.maj.ast.ASTNode
import com.maj.emitters.{BufferEmitter, IREmitter}
import com.maj.ir.{IRGenerator, IRNode}
import com.maj.parser._
import com.maj.typecheck.TypeChecker


// TODO: Optimizations IR
// TODO: Inline ASM parsing
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
    //    val emitter: StringBufferEmitter = new StringBufferEmitter()
    val irEmitter: BufferEmitter[IRNode] = new IREmitter()
    val typeCheck = new TypeChecker()
    val irgen = new IRGenerator()(irEmitter)
    val ast: ASTNode = Token.parser.parseWithStdLib(file)
    typeCheck.visit(ast)
    irgen.visit(ast)

    irEmitter.writeToFile(s"$output.maj.ir")
  }
}


