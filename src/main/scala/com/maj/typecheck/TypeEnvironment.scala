package com.maj.typecheck

import com.maj.ast.{MajTypeUndefined, TypeNode}

class TypeEnvironment(val parent: TypeEnvironment = null) {
  private var types: Map[String, TypeNode] = Map()

  def addType(name: String, t: TypeNode): Unit = {
    types += (name -> t)
  }

  def getType(name: String): TypeNode = {
    types.getOrElse(name, getParent(name))
  }

  def getParent(name: String): TypeNode = {
    if (parent == null) {
      MajTypeUndefined()
    } else {
      parent.getType(name)
    }
  }

  def getOrThrow(name: String): TypeNode = {
    this.getType(name) match {
      case MajTypeUndefined() => throw new RuntimeException(s"Type $name not found")
      case t => t
    }
  }
}
