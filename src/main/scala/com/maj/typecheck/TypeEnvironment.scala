package com.maj.typecheck

class TypeEnvironment(val parent: TypeEnvironment = null) {
  private var types: Map[String, TypeNode] = Map()
  private var scope: Option[String] = None

  def addType(name: String, t: TypeNode): Unit = {
    types += (name -> t)
  }

  def getType(name: String): Option[TypeNode] = {
    types.get(name).orElse(getParentType(name))
  }

  private def getParentType(name: String): Option[TypeNode] = {
    if (parent == null) {
      None
    } else {
      parent.getType(name)
    }
  }


  def setScope(newScope: String): Unit = {
    scope = Some(newScope)
  }

  def getScope: Option[String] = scope.orElse(getParentScope)

  private def getParentScope: Option[String] = {
    if (parent == null) {
      None
    } else {
      parent.getScope
    }
  }

  def getTypeOrThrow(name: String): TypeNode = {
    this.getType(name) match {
      case None => throw new RuntimeException(s"Type $name not found")
      case Some(t) => t
    }
  }

}
