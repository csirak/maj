package com.maj.parser

import scala.util.matching.Regex

case class Source(val input: String, val index: Int) {
  def check(regex: Regex): Option[ParseResult[String]] = {
    val matcher = regex.pattern.matcher(input)
    matcher.region(index, input.length)
    println(s"tried: $regex")
    println(s"source: $this")

    if (matcher.lookingAt()) {
      val value = matcher.group()
      val newIndex = index + value.length
      val source = new Source(input, newIndex)
      println(s"matched: $value")
      Some(ParseResult(value, source))
    } else {
      None
    }

  }

  override def toString(): String = {
    s"\"${input.substring(index)}\""
  }

  def atEnd(): Boolean = input.length == index
}

//Some(
//  ParseResult(
//    (
//      Numeric(2),
//      List(
//        (
//          *,
//          Numeric(2)
//        )
//      )
//    ),
//    ""
//  )
//)