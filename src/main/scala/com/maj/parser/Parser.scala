package com.maj.parser

import scala.annotation.tailrec

case class ParseResult[+T](value: T, source: Source)

class Parser[+T](val parse: Source => Option[ParseResult[T]]) {
  def apply(): Parser[T] = this

  def or[U >: T](parser: Parser[U]): Parser[U] = new Parser(source => parse(source).orElse(parser.parse(source)))

  def and[U](parser: Parser[U]): Parser[U] = bind(_ => parser())

  def map[U](callback: T => U): Parser[U] = bind(value => Parser.constant(callback(value)))

  def bind[U](callback: T => Parser[U]): Parser[U] = new Parser[U](source =>
    parse(source) match {
      case Some(ParseResult(value, nextSource)) => callback(value).parse(nextSource)
      case None => None
    }
  )

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
      case Some(ParseResult(value, remainingSource)) =>
        if (remainingSource.atEnd()) value
        else throw new Exception(s"Parse error: Incomplete input at index ${remainingSource.index}\nRemaining input: ${remainingSource}")
      case None => throw new Exception("Parse error: Unable to parse input")
    }
  }
<<<<<<< Updated upstream
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
=======

  def zeroOrMore: Parser[List[T]] = new Parser(source => {
    @tailrec
    def recursiveParse(source: Source, acc: List[T]): Option[ParseResult[List[T]]] = {
      this.parse(source) match {
>>>>>>> Stashed changes
        case Some(ParseResult(value, nextSource)) => recursiveParse(nextSource, acc :+ value)
        case None => Some(ParseResult(acc, source))
      }
    }

    recursiveParse(source, List.empty)
  })

  def parseFile(path: String, log: Boolean = false): ASTNode = {
    val sourceFile = scala.io.Source.fromFile(path, "utf-8")
    val lines = sourceFile.getLines.mkString("\n")
    val ast = Token.parser.parseToCompletion(lines, log)
    sourceFile.close()
    ast
  }
}

object Parser {
  def constant[U](value: U): Parser[U] = new Parser(source => Some(ParseResult(value, source)))

  def error[U](message: String): Parser[U] = new Parser(_ => throw new Exception(message))

  def zeroOrMore[U](parser: Parser[U]): Parser[List[U]] = new Parser(source => {
    @tailrec
    def recursiveParse(source: Source, acc: List[U]): Option[ParseResult[List[U]]] = {
      parser.parse(source) match {
        case Some(ParseResult(value, nextSource)) => recursiveParse(nextSource, acc :+ value)
        case None => Some(ParseResult(acc, source))
      }
    }

    recursiveParse(source, List.empty)
  })
}
