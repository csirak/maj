import com.maj.ast.ASTNode
import com.maj.emitters.BufferEmitter
import com.maj.ir.{IRGenerator, IRNode}
import com.maj.parser._


// TODO: Optimizations IR
//    TODO: Constants IR
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
    //    implicit val emitter: StringBufferEmitter = new StringBufferEmitter()
    implicit val irEmitter: BufferEmitter[IRNode] = new BufferEmitter[IRNode]()
    //    val codeGen = new CodeGenerator()
    //    val typeCheck = new TypeChecker()
    val irgen = new IRGenerator()
    val ast: ASTNode = Token.parser.parseFile(file)
    //    typeCheck.visit(ast)
    //    codeGen.visit(ast)
    irgen.visit(ast)

    irEmitter.output.map(_.toString).map(line => if (line.contains(":")) line else "\t\t" ++ line).foreach(println)
    //    emitter.writeToFile(s"$output.s")
  }
}


