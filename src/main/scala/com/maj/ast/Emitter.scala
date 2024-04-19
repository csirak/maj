package com.maj.ast

import java.io.PrintWriter

trait Emitter {
  def emit(output: String): Unit

  def emitLine(output: String): Unit

}

class StdOutEmitter extends Emitter {
  def emit(output: String): Unit = println(output)

  def emitLine(output: String): Unit = println(s"\t\t$output")
}


class AsmEmitter extends Emitter {
  private var buffer = List.empty[String]

  def emit(output: String): Unit = {
    buffer = buffer :+ output
  }

  def emitLine(output: String): Unit = buffer = buffer :+ (s"\t\t$output")

  def getOutput(fileName: String): PrintWriter = {
    new PrintWriter(fileName) {
      write(buffer.mkString("\n"));
      close()
    }

  }
}