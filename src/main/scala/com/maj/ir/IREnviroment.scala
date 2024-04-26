package com.maj.ir

import scala.collection.mutable


class IREnviroment(parent: IREnviroment = null) {
  private var symbolTable: Map[String, Int] = Map()
  private val freeVars: mutable.Set[Int] = mutable.HashSet()
  private val inUse: mutable.Set[Int] = mutable.HashSet()
  private var labelCounter: Int = 0
  private var anonCount = 0

  def getVar(name: String): IdenIR = {
    IdenIR(symbolTable.getOrElse(name, throw new Exception(s"Variable not found: $name")))
  }

  def addVar(name: String): IdenIR = {
    if (symbolTable.contains(name)) {
      throw new Exception(s"Variable already exists: $name")
    }
    val varIndex = addAnonVar()
    symbolTable += (name -> varIndex.symbolIndex)
    varIndex
  }


  def addAnonVar(): IdenIR = {
    val varIndex = if (freeVars.isEmpty) anonCount else freeVars.head
    inUse += varIndex
    anonCount += 1
    IdenIR(varIndex)
  }


  def nextLabel: LabelIR = {
    labelCounter += 1
    new LabelIR(labelCounter)
  }

}
