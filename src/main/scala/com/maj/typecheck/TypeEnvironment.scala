package com.maj.typecheck

class TypeEnvironment(val parent: TypeEnvironment = null) {
  private var types: Map[String, TypeNode] = Map()

  def addType(name: String, t: TypeNode): Unit = {
    types += (name -> t)
  }

  def getType(name: String): Option[TypeNode] = {
    types.get(name).orElse(getParent(name))
  }

  private def getParent(name: String): Option[TypeNode] = {
    if (parent == null) {
      None
    } else {
      parent.getType(name)
    }
  }

  def getOrThrow(name: String): TypeNode = {
    this.getType(name) match {
      case None => throw new RuntimeException(s"Type $name not found")
      case Some(t) => t
    }
  }
}
