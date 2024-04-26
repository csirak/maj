package com.maj.parser

import com.maj.ast.{ASTNode, Block}

import scala.annotation.tailrec

case class ParseResult[+T](val value: T, val source: Source)

class Parser[+T](val parse: Source => Option[ParseResult[T]]) {
  def apply(): Parser[T] = this

  def or[U >: T](parser: Parser[U]): Parser[U] = {
    new Parser(source => {
      val result = parse(source);
      result.orElse(parser.parse(source))
    })
  }

  def or[U >: T](parsers: List[Parser[U]]): Parser[U] = {
    if (parsers.isEmpty) {
      throw new Exception("Cannot or an empty list of parsers")
    }
    parsers.foldLeft(parsers.head)((acc, parser) => acc.or(parser))
  }


  def and[U](parser: Parser[U]): Parser[U] = {
    bind(_ => parser())
  }

  def map[U](callback: (T) => U): Parser[U] = {
    bind((value) => {
      Parser.constant(callback(value))
    })
  }

  def bind[U](callback: (T) => Parser[U]): Parser[U] = {
    new Parser[U](source => {
      val result = parse(source)
      result match {
        case Some(ParseResult(value, nextSource)) => callback(value).parse(nextSource)
        case _ => None
      }
    })
  }

  def optional[U >: T]: Parser[Option[U]] = new Parser(source =>
    parse(source) match {
      case Some(ParseResult(value, nextSource)) => Some(ParseResult(Some(value), nextSource))
      case None => Some(ParseResult(None, source))
    }
  )

  def flatMap[U]: (T => Parser[U]) => Parser[U] = bind[U]

  private def parseToCompletion(string: String, log: Boolean = false): T = {
    val source = Source(string, 0, log)
    parse(source) match {
      case Some(ParseResult(value, remainingSource)) => if (remainingSource.atEnd()) value else throw new Exception(s"Parse error: Incomplete input at index ${remainingSource.index}\n Remaining input: ${remainingSource}")
      case None => throw new Exception("Parse error: Unable to parse input")
    }
  }

  def zeroOrMore: Parser[List[T]] = new Parser(source => {
    @tailrec
    def recursiveParse(source: Source, acc: List[T]): Option[ParseResult[List[T]]] = {
      this.parse(source) match {
        case Some(ParseResult(value, nextSource)) => recursiveParse(nextSource, acc :+ value)
        case None => Some(ParseResult(acc, source))
      }
    }

    recursiveParse(source, List.empty)
  })

  private def stdLib = parseFile("src/main/resources/stdlib.maj")

  private def parseFile(path: String, log: Boolean = false): Block = {
    val sourceFile = scala.io.Source.fromFile(path, "utf-8")
    val lines = sourceFile.getLines.mkString("\n")
    sourceFile.close()
    Token.parser.parseToCompletion(lines, log)
  }

  def parseWithStdLib(path: String, log: Boolean = false): ASTNode = stdLib ++ parseFile(path, log)
}


object Parser {
  def constant[U](value: U): Parser[U] = {
    new Parser(source => Some(new ParseResult(value, source)))
  }

  def error[U](message: String): Parser[U] = {
    new Parser(_ => {
      throw new Exception(message)
    })
  }

  def zeroOrMore[U](parser: Parser[U]): Parser[List[U]] = {
    @tailrec def recursiveParse(source: Source, acc: List[U]): ParseResult[List[U]] = {
      parser.parse(source) match {
        case Some(ParseResult(value, nextSource)) => recursiveParse(nextSource, acc :+ value)
        case None => ParseResult(acc, source)
      }
    }

    new Parser(source => Some(recursiveParse(source, List.empty)))
  }
}



