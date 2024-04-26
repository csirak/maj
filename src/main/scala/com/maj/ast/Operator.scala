package com.maj.ast


abstract class Operator[T] {
  def left: T

  def right: T

  def get(left: T, right: T): T

  def getType: Operator[T]
}
