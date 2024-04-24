package com.maj.codegen

case class OffsetEnvironment(val parent: OffsetEnvironment = null) {
  private var localOffset: Int = 0
  private var locals: Map[String, Int] = Map()


  def addLocalWithOffset(name: String, offset: Int): Unit = {
    localOffset += offset
    locals += (name -> (localOffset))
  }


  def get(name: String): Int = locals.getOrElse(name, getParent(name))

  def getParent(name: String): Int = {
    if (parent == null) {
      -1
    } else {
      parent.get(name)
    }
  }
}
