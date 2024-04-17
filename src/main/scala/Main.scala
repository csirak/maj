import com.maj.ast._
import com.maj.parser._

object Main {


  def main(args: Array[String]): Unit = {
    //    val expressionParser = Parser.zeroOrMore(Token.atom).parseToCompletion("hello1 bye2 hello2").map(println)
    val lines = scala.io.Source.fromFile("src/main/resources/test.maj", "utf-8").getLines.mkString("\n")
    val source = new Source(lines, 0)

    val out: Option[ParseResult[ASTNode]] = Token.parser.parse(source)
    println(out.get.value)
  }

}