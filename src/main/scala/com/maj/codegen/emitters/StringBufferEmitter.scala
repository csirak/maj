package com.maj.codegen.emitters

import java.io.PrintWriter

class StringBufferEmitter extends Emitter {
  private var buffer = List.empty[String]

  def emit(output: String): Unit = {
    buffer = buffer :+ output
  }

  def emitLine(output: String): Unit = buffer = buffer :+ (s"\t\t$output")

  def writeToFile(fileName: String): PrintWriter = {
    new PrintWriter(fileName) {
      write(buffer.mkString("\n"));
      close()
    }

  }
}
