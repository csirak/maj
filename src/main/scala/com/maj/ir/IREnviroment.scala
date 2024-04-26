package com.maj.ir


class IREnviroment(parent: IREnviroment = null) {
  private var symbolTable: Map[String, Int] = Map()
  private var constants: Map[String, IRNode] = Map()
  private var labelCounter: Int = 0
  private var anonCount = 0

  def getVar(name: String): IdenIR = {
    symbolTable.get(name).map(IdenIR).getOrElse(throw new Exception(s"Variable not found: $name"))
  }

  def getValue(name: String): IRNode = {
    symbolTable.get(name).map(IdenIR).orElse(constants.get(name)).getOrElse(throw new Exception(s"Variable not found: $name"))
  }

  def addVar(name: String): IdenIR = {
    if (symbolTable.contains(name)) {
      throw new Exception(s"Variable already exists: $name")
    }
    val varIndex = addAnonVar()
    symbolTable += (name -> varIndex.symbolIndex)
    varIndex
  }

  def addConst(name: String, node: IRNode): Unit = {
    val tableEntry = node match {
      case node: ScalarIR => node
      case node: IdenIR => node
      case _ => throw new Exception("Invalid Constant")
    }
    constants += (name -> tableEntry)
  }


  def addAnonVar(): IdenIR = {

    anonCount += 1
    IdenIR(anonCount)
  }

  def setVar(name: String, newIden: IdenIR): IdenIR = {
    symbolTable += (name -> newIden.symbolIndex)
    newIden
  }

  def nextLabel: LabelIR = {
    labelCounter += 1
    new LabelIR(labelCounter)
  }


}
