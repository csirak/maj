package com.maj.parser

import com.maj.ast.ASTNode

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

  def parseToCompletion(string: String, log: Boolean = false): T = {
    val source = Source(string, 0, log)
    parse(source) match {
      case Some(ParseResult(value, remainingSource)) => if (remainingSource.atEnd()) value else throw new Exception(s"Parse error: Incomplete input at index ${remainingSource.index}\n Remaining input: ${remainingSource}")
      case None => throw new Exception("Parse error: Unable to parse input")
    }
  }

  def parseFile(path: String): ASTNode = {
    val sourceFile = scala.io.Source.fromFile(path, "utf-8")
    val lines = sourceFile.getLines.mkString("\n")
    val ast = Token.parser.parseToCompletion(lines)
    sourceFile.close()
    ast
  }

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



