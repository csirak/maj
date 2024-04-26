package com.maj.emitters

class StringBufferEmitter extends BufferEmitter[String] {
  override def transform(output: String): String = s"\t\t$output"
}
