package com.maj.parser

import scala.util.matching.Regex

case class Source(val input: String, val index: Int, log: Boolean = false) {
  def check(regex: Regex): Option[ParseResult[String]] = {
    val matcher = regex.pattern.matcher(input).region(index, input.length)

    println(s"tried ${regex}")
    if (matcher.lookingAt()) {
      val value = matcher.group()

      println(s"matched ${value}")

      val newIndex = index + value.length
      val source = new Source(input, newIndex, log)
      println(s"source: ${source}")
      Some(ParseResult(value, source))
    } else {
      None
    }
  }

  override def toString: String = {
    s"\"${input.substring(index)}\""
  }

  def atEnd(): Boolean = input.length == index
}
