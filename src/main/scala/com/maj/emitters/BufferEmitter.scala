package com.maj.emitters

class BufferEmitter[T] extends Emitter[T] {
  private var buffer = List.empty[T]

  def output: List[T] = buffer

  def emit(output: T): Unit = {
    buffer = buffer :+ output
  }

  def emitLine(output: T): Unit = emit(transform(output))

  def transform(output: T): T = output
}
