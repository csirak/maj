package com.maj.emitters

trait Emitter {
  def emit(output: String): Unit

  def emitLine(output: String): Unit
}
