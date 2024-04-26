package com.maj.emitters

import com.maj.ir.IRNode

import java.io.PrintWriter

class IREmitter extends BufferEmitter[IRNode] {
  override def writeToFile(fileName: String): PrintWriter = {
    new PrintWriter(fileName) {
      write(output.map(_.toString).map(line => if (line.contains(":")) line else "\t\t" ++ line).mkString("\n"));
      close()
    }
  }
}
