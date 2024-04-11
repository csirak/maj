import com.maj.parser._

object Main {


  def main(args: Array[String]): Unit = {
    //    val expressionParser = Parser.zeroOrMore(Token.atom).parseToCompletion("hello1 bye2 hello2").map(println)
    val source = new Source("(id + (!13))  == 0", 0)

    //    source.check("\\(".r).map(println)
    val out = Token.expression.parse(source)
    println(out)
  }

}