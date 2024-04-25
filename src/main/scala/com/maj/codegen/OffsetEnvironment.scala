package com.maj.codegen

import com.maj.ast.ASTNode

case class OffsetEnvironment(val parent: OffsetEnvironment = null) {
  private var localOffset: Int = 0
  private var locals: Map[String, Int] = Map()
  private var constants: Map[String, ASTNode] = Map()


  def addLocalWithOffset(name: String, offset: Int): Unit = {
    localOffset += offset
    locals += (name -> (localOffset))
  }

  def addConstant(name: String, value: ASTNode): Unit = {
    constants += (name -> value)
  }


  def get(name: String): Option[Int] = locals.get(name).orElse(getParent(name))

  private def getParent(name: String): Option[Int] = {
    if (parent == null) {
      None
    } else {
      parent.get(name)
    }
  }

  def getConstant(name: String): Option[ASTNode] = constants.get(name).orElse(getConstantParent(name))

  private def getConstantParent(name: String): Option[ASTNode] = {
    if (parent == null) {
      None
    } else {
      parent.getConstant(name)
    }
  }

  def getValue(name: String): Option[Either[Int, ASTNode]] = {
    get(name).map(Left(_)).orElse(getConstant(name).map(Right(_)))
  }
}
