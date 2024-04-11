package com.maj.parser

import com.maj.ast._


object Token {
  implicit class StringRegexAddition(val s: String) extends AnyVal {
    def regexp: Parser[String] = {
      Parser.regexp(s.r)
    }
  }

  val WHITESPACE = "[ \\n\\r\\t]+".regexp
  val COMMENTS = "//.*/".regexp.or("/*.*[*]/".regexp)
  val IGNORED = () => Parser.zeroOrMore(WHITESPACE.or(COMMENTS))

  def clean(pattern: String) = {
    pattern.regexp.bind(value => {
      IGNORED().bind(sp => {
        Parser.constant(value)
      })
    })
  }

  val FUNCTION = clean("func")
  val IF = clean("if")
  val ELSE = clean("else")
  val RETURN = clean("return")
  val VAR = clean("var")
  val WHILE = clean("while")

  private val COMMA = clean("[,]")
  val SEMICOLON = clean(";")
  private val LEFT_PAREN: Parser[String] = clean("\\(")
  private val RIGHT_PAREN: Parser[String] = clean("\\)")
  val LEFT_CURLY = clean("[{]")
  val RIGHT_CURLY = clean("[}]")

  // TODO: Add hex and bin encoding
  private val NUMBER: Parser[ASTNode] = clean("[0-9]+").map(digits => Numeric(digits.toInt))

  private val ID = clean("[a-zA-Z_][a-zA-Z0-9_]*")

  val NOT: Parser[Not] = clean("!").map(_ => Not())
  val EQUAL: Parser[Operator] = clean("==").map(_ => Equals())
  val NOT_EQUAL: Parser[Operator] = clean("!=").map(_ => NotEquals())
  val ADD: Parser[Operator] = clean("\\+").map(_ => Add())
  val SUB: Parser[Operator] = clean("\\-").map(_ => Sub())
  val MUL: Parser[Operator] = clean("\\*").map(_ => Mul())
  val DIV: Parser[Operator] = clean("\\/").map(_ => Div())

  val id: Parser[ASTNode] = ID.map(name => Iden(name))
  val scalar = NUMBER.or(id).or(call)
  var expression = scalar

  val arguments = expression.bind(arg => {
    Parser.zeroOrMore(COMMA.and(() => expression)).bind(args => {
      Parser.constant(arg :: args)
    }).or(Parser.constant(List.empty))
  })

  val call: Parser[ASTNode] = ID.bind(name => {
    LEFT_PAREN.and(() => arguments).bind(args => {
      RIGHT_PAREN.and(() => Parser.constant(Call(name, args)))
    })
  })

  expression = (LEFT_PAREN.bind(par => {
    expression.bind(
      expr => {
        RIGHT_PAREN.and(() => Parser.constant(expr))
      })
  })).or(expression)


  val unary: Parser[ASTNode] = NOT.bind((not) =>
    expression.bind((exp) => Parser.constant(not.get(exp)))
  )
  expression = unary.or(expression)

  val infix = (operator: Parser[Operator], operandFn: () => Parser[ASTNode]) => {
    val operand = operandFn
    operand().bind(first => {
      Parser.zeroOrMore(operator.bind(op => {
        operand().bind(exp => {

          Parser.constant((op, exp))
        })
      })).bind(ops => {
        Parser.constant(
          ops.foldLeft(first)((acc, opExp) => {
            val (op, exp) = opExp
            op.get(acc, exp)
          }))
      })
    })
  }

  val product = infix(MUL.or(DIV), () => expression)
  expression = product.or(expression)
  val sum = infix(ADD.or(SUB), () => expression)
  expression = sum.or(expression)
  val comparison = infix(EQUAL.or(NOT_EQUAL), () => expression)
  expression = comparison.or(expression)
}