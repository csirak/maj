package com.maj.codegen

case class Label(val value: Int = 0) {
  override def toString: String = s".L$value"
}

object Label {
  private var counter: Int = 0

  def next: Label = {
    counter += 1
    new Label(counter)
  }
}
