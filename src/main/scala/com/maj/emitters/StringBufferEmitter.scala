package com.maj.emitters

import java.io.PrintWriter

class StringBufferEmitter extends BufferEmitter[String] {
  override def transform(output: String): String = s"\t\t$output"

  def writeToFile(fileName: String): PrintWriter = {
    new PrintWriter(fileName) {
      write(output.mkString("\n"));
      close()
    }
  }
}
