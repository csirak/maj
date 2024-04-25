package com.maj.typecheck

import com.maj.ast.Operator

sealed trait TypeNode {
  def accepts(other: TypeNode): Boolean = {
    this == other
  }

  def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => Option[TypeNode]): Boolean = {
    resolver(this) match {
      case Some(node: TypeOperator) => node.acceptsWithResolver(other, resolver)
      case Some(node) => node.accepts(other)
      case None => false
    }
  }
}

case class MajIntType() extends TypeNode

case class MajBoolType() extends TypeNode

case class MajVoidType() extends TypeNode

case class MajCharType() extends TypeNode

case class MajTypeUndefined() extends TypeNode

case class MajFuncType(val returnType: TypeNode, val params: List[TypeNode]) extends TypeNode {
  override def accepts(other: TypeNode): Boolean = {
    other match {
      case MajFuncType(otherReturnType, otherParams) => {
        returnType == otherReturnType && params == otherParams
      }
      case _ => false
    }
  }

  override def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => Option[TypeNode]): Boolean = {
    other match {
      case MajFuncType(otherReturnType, otherParams) => {
        returnType.acceptsWithResolver(otherReturnType, resolver) && params.zip(otherParams).forall {
          case (param, otherParam) => param.acceptsWithResolver(otherParam, resolver)
        }
      }
      case _ => false
    }
  }
}


case class MajConstant(val typ: TypeNode) extends TypeNode {
  override def accepts(other: TypeNode): Boolean = typ.accepts(other)

  override def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => Option[TypeNode]): Boolean = typ.acceptsWithResolver(other, resolver)
}

case class MajStruct(val name: String, val fields: Map[String, String]) extends TypeNode

case class MajType(val typ: String) extends TypeNode {
  override def toString: String = typ
}

abstract class ReturnableType extends TypeNode {
  def returnType: TypeNode

  override def accepts(other: TypeNode): Boolean = {
    returnType.accepts(other)
  }

  override def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => Option[TypeNode]): Boolean = {
    resolver(other) match {
      case Some(node: TypeOperator) => node.acceptsWithResolver(other, resolver)
      case None => false
    }
  }
}

case class MajReturnType(val returnType: TypeNode) extends ReturnableType

case class MajConditionalReturn(val returnType: TypeNode) extends ReturnableType

abstract class TypeOperator extends Operator[TypeNode] with TypeNode {
  def left: TypeNode

  def right: TypeNode

  def get(left: TypeNode, right: TypeNode): TypeOperator
}

case class MajTypeComposeOr(left: TypeNode = null, right: TypeNode = null) extends TypeOperator {
  override def accepts(other: TypeNode): Boolean = {
    other.accepts(left) || other.accepts(right)
  }

  override def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => Option[TypeNode]): Boolean = {
    resolver(other) match {
      case Some(node) => node.acceptsWithResolver(left, resolver) || node.acceptsWithResolver(right, resolver)
    }
  }

  override def get(left: TypeNode, right: TypeNode): TypeOperator = MajTypeComposeOr(left, right)
}

case class MajTypeComposeAnd(left: TypeNode = null, right: TypeNode = null) extends TypeOperator {


  override def get(left: TypeNode, right: TypeNode): TypeOperator = MajTypeComposeAnd(left, right)

  override def acceptsWithResolver(other: TypeNode, resolver: (TypeNode) => Option[TypeNode]): Boolean = {
    resolver(other) match {
      case Some(node) => node.acceptsWithResolver(left, resolver) && node.acceptsWithResolver(right, resolver)
    }
  }
}
