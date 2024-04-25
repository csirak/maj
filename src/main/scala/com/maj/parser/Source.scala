package com.maj.parser

import scala.util.matching.Regex

case class Source(val input: String, val index: Int, log: Boolean = false) {
  def check(regex: Regex): Option[ParseResult[String]] = {
    val matcher = regex.pattern.matcher(input).region(index, input.length)

    if (log) outputReg(regex)
    if (matcher.lookingAt()) {
      val value = matcher.group()
      val newIndex = index + value.length
      val source = new Source(input, newIndex, log)
      if (log) println(s"check: ${value} -> ${source}")
      Some(ParseResult(value, source))
    } else {
      None
    }
  }

  override def toString: String = {

    val split = input.substring(index).split('\n')
    if (split.isEmpty) "" else split.head

  }

  private def outputReg(reg: Regex): Unit = {
    reg.toString() match {
      case Token.whitespaceRegex =>
      case Token.singleCommentRegex =>
      case Token.multiCommentRegex =>
      case _ => println(s"tried ${reg}")
    }


  }

  def atEnd(): Boolean = input.length == index
}
