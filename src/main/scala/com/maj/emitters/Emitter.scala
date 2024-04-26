package com.maj.emitters

trait Emitter[T] {
  def emit(emitted: T): Unit

  def emitLine(emitted: T): Unit
}
