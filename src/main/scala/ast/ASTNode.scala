package com.maj.ast

trait ASTNode {
  def equals(ASTNode: node): Boolean;
  def getTypeSig():String;
}
