package com.maj.codegen

case class Environment() {
  private var locals: Map[String, Int] = Map()

  def addLocal(name: String, offset: Int): Unit = {
    locals += (name -> offset)
  }

  def get(name: String): Int = locals.getOrElse(name, -1)
}
