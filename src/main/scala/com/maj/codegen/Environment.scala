package com.maj.codegen

case class Environment() {
  private var localOffset: Int = 0
  private var locals: Map[String, Int] = Map()

  def addLocal(name: String, offset: Int): Unit = {
    locals += (name -> (offset))
  }

  def addLocalWithOffset(name: String, offset: Int): Unit = {
    localOffset += offset
    locals += (name -> (localOffset))
  }


  def get(name: String): Int = locals.getOrElse(name, -1)

  def updateOffset(offset: Int): Unit = {
    localOffset = offset
  }
}
