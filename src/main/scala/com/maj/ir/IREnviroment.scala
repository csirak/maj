package com.maj.ir


class IREnviroment(parent: IREnviroment = null) {
  private var symbolTable: Map[String, Int] = Map()
  private var constants: Map[String, IRNode] = Map()
  private var labelCounter: Int = 0
  private var anonCount = 0

  def getVar(name: String): IRIden = {
    getOrThrow(symbolTable.get(name).map(IRIden), name)
  }

  def getValue(name: String): IRNode = {
    getOrThrow(symbolTable.get(name).map(IRIden).orElse(constants.get(name)), name)
  }

  private def getOrThrow[T <: IRNode](node: Option[T], name: String): T = {
    node.getOrElse(throw new Exception(s"Variable not found: $name in scope"))
  }

  def addVar(name: String): IRIden = {
    if (symbolTable.contains(name)) {
      throw new Exception(s"Variable already exists: $name")
    }
    val varIndex = addAnonVar()
    symbolTable += (name -> varIndex.symbolIndex)
    varIndex
  }

  def addConst(name: String, node: IRNode): Unit = {
    val tableEntry = node match {
      case node: IRScalar => node
      case node: IRIden => node
      case _ => throw new Exception("Invalid Constant")
    }
    constants += (name -> tableEntry)
  }


  def addAnonVar(): IRIden = {

    anonCount += 1
    IRIden(anonCount)
  }

  def setVar(name: String, newIden: IRIden): IRIden = {
    symbolTable += (name -> newIden.symbolIndex)
    newIden
  }

  def nextLabel: IRLabel = {
    labelCounter += 1
    new IRLabel(labelCounter)
  }


}
