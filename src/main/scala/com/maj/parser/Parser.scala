package com.maj.parser

import scala.util.matching.Regex

case class ParseResult[+T](val value: T, val source: Source)

case class Parser[T](val parse: Source => Option[ParseResult[T]]) {
  def apply(): Parser[T] = this

  def or(parser: Parser[T]): Parser[T] = {
    new Parser(source => {
      val result = parse(source);
      result.orElse(parser.parse(source))
    })
  }

  def and[U](parser: () => Parser[U]): Parser[U] = {
    bind((_) => parser())
  }

  def map[U](callback: (T) => U): Parser[U] = {
    bind((value) => {
      Parser.constant(callback(value))
    })
  }

  def bind[U](callback: (T) => Parser[U]): Parser[U] = {
    new Parser[U]((source) => {
      val result = parse(source)
      result match {
        case Some(ParseResult(value, nextSource)) => callback(value).parse(nextSource)
        case _ => None
      }
    })
  }

  def parseToCompletion(string: String): T = {
    val source = Source(string, 0)
    parse(source) match {
      case Some(ParseResult(value, remainingSource)) =>
        if (remainingSource.atEnd) {
          value
        } else {
          val errorIndex = remainingSource.index
          throw new Exception(s"Parse error: Incomplete input at index ${errorIndex}")
        }
      case None =>
        throw new Exception("Parse error: Unable to parse input")
    }
  }
}


object Parser {
  def regexp(regex: Regex): Parser[String] = {
    new Parser(source => {
      source.check(regex)
    })
  }

  def constant[U](value: U): Parser[U] = {
    new Parser(source => Some(new ParseResult(value, source)))
  }

  def error[U](message: String): Parser[U] = {
    new Parser(source => {
      throw new Exception(message)
    })
  }

  def zeroOrMore[U](parser: Parser[U]): Parser[List[U]] = {
    def recursiveParse(source: Source, acc: List[U]): ParseResult[List[U]] = {
      parser.parse(source) match {
        case Some(ParseResult(value, nextSource)) => recursiveParse(nextSource, acc :+ value)
        case None => ParseResult(acc, source)
      }
    }

    new Parser(source => Some(recursiveParse(source, List.empty)))
  }
}



