package com.maj.parser

import com.maj.ast._


object Token {
  implicit class StringRegexAddition(val s: String) extends AnyVal {
    def regexp: Parser[String] = {
      Parser.regexp(s.r)
    }
  }

  private val WHITESPACE = "[ \\n\\r\\t]+".regexp
  private val COMMENTS = "//.*/".regexp.or("/*.*[*]/".regexp)
  private val IGNORED = Parser.zeroOrMore(WHITESPACE.or(COMMENTS))

  private def clean(pattern: String) = {
    pattern.regexp.bind(value => {
      IGNORED.bind(sp => {
        Parser.constant(value)
      })
    })
  }

  private val FUNCTION = clean("func")
  private val IF = clean("if")
  private val ELSE = clean("else")
  private val RETURN = clean("return")
  private val VAR = clean("var")
  private val WHILE = clean("while")

  private val COMMA = clean("[,]")
  private val ASSIGN = clean("=")
  private val SEMICOLON = clean(";")
  private val LEFT_PAREN: Parser[String] = clean("\\(")
  private val RIGHT_PAREN: Parser[String] = clean("\\)")
  private val LEFT_CURLY = clean("\\{")
  private val RIGHT_CURLY = clean("\\}")

  // TODO: Add hex and bin encoding
  private val NUMBER: Parser[ASTNode] = clean("[0-9]+").map(digits => Numeric(digits.toInt))

  private val ID = clean("[a-zA-Z_][a-zA-Z0-9_]*")

  private val NOT: Parser[Not] = clean("\\!").map(_ => Not())
  private val EQUAL: Parser[Operator] = clean("==").map(_ => Equals())
  private val NOT_EQUAL: Parser[Operator] = clean("!=").map(_ => NotEquals())
  private val ADD: Parser[Operator] = clean("\\+").map(_ => Add())
  private val SUB: Parser[Operator] = clean("\\-").map(_ => Sub())
  private val MUL: Parser[Operator] = clean("\\*").map(_ => Mul())
  private val DIV: Parser[Operator] = clean("\\/").map(_ => Div())
  private val MOD: Parser[Operator] = clean("\\%").map(_ => Mod())

  private val id: Parser[ASTNode] = ID.map(name => Iden(name))
  private var expression: Parser[ASTNode] = id.or(NUMBER)

  private val arguments = (expression: Parser[ASTNode]) => expression.bind(arg => {
    Parser.zeroOrMore(COMMA.and(expression)).bind(args => {
      Parser.constant(arg :: args)
    }).or(Parser.constant(List.empty))
  })

  private val call: Parser[ASTNode] = ID.bind(name => {
    LEFT_PAREN.bind(_ => arguments(expression).bind(args => {
      RIGHT_PAREN.and(if (name == "assert") Parser.constant(Assert(args.head)) else Parser.constant(Call(name, args)))
    }).or(RIGHT_PAREN.and(Parser.constant(Call(name, List.empty)))))
  })

  expression = call.or(expression)

  expression = LEFT_PAREN.bind(par => {
    expression.bind(
      expr => {
        RIGHT_PAREN.and(Parser.constant(expr))
      })
  }).or(expression)

  private val unary: Parser[ASTNode] = NOT.bind((not) =>
    expression.bind((exp) => Parser.constant(not.get(exp)))
  )
  expression = unary.or(expression)

  private val infix = (operator: Parser[Operator], operandFn: Parser[ASTNode]) => {
    val operand = operandFn
    operand.bind(first => {
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

  private val product = infix(MUL.or(DIV).or(MOD), expression)
  expression = product.or(expression)
  private val sum = infix(ADD.or(SUB), expression)
  expression = sum.or(expression)
  private val comparison = infix(EQUAL.or(NOT_EQUAL), expression)
  expression = comparison.or(expression)


  private var expressionStatement = expression.bind(exp => SEMICOLON.and(Parser.constant(exp)))
  private val returnStatement = RETURN.and(expressionStatement).bind(exp => Parser.constant(Return(exp)))


  private val varStatement: Parser[ASTNode] = VAR.bind(_ => ID.bind(name => {
    ASSIGN.and(expression.bind(value => SEMICOLON.and(Parser.constant(Create(name, value)))))
  }))

  private val assignStatement = ID.bind(name => {
    ASSIGN.and(expression).bind(value => SEMICOLON.and(Parser.constant(Assign(name, value))))
  })


  private val blockStatement = (statement: Parser[ASTNode]) => LEFT_CURLY.and(Parser.zeroOrMore(statement).bind(body => {
    RIGHT_CURLY.and(Parser.constant(Block(body)))
  }))

  private var statement: Parser[ASTNode] = (varStatement).or(assignStatement).or(expressionStatement).or(returnStatement).or(blockStatement(statement))


  lazy private val elseStatement: Parser[ASTNode] = ELSE.and(blockStatement(statement).bind(block => {
    Parser.constant(block)
  }))


  lazy private val ifElseStatement: Parser[ASTNode] = ELSE.and(IF).and(LEFT_PAREN).bind(_ => expression.bind(cond => {
    RIGHT_PAREN.and(blockStatement(statement).bind(block => {
      ifElseStatement.or(elseStatement).bind(statement => {
        Parser.constant(Conditional(cond, block, Some(statement)))
      }).or(Parser.constant(Conditional(cond, block, None)))
    }))
  }))

  lazy private val ifStatement: Parser[ASTNode] = IF.and(LEFT_PAREN).bind(_ => expression.bind(cond => {
    RIGHT_PAREN.and(blockStatement(statement).bind(block => {
      elseStatement.bind(statement => {
        Parser.constant(Conditional(cond, block, Some(statement)))
      }).or(Parser.constant(Conditional(cond, block, None)))
    }))
  }))

  statement = ifStatement.or(statement)


  lazy private val whileStatement: Parser[ASTNode] = WHILE.and(LEFT_PAREN).bind(_ => expression.bind(cond => {
    RIGHT_PAREN.and(blockStatement(statement).bind(block => {
      Parser.constant(Loop(cond, block))
    }))
  }))

  statement = whileStatement.or(statement)

  lazy private val paramsStatement = ID.bind(param => {
    Parser.zeroOrMore(COMMA.and(ID)).bind(params => {
      Parser.constant(param :: params)
    })
  }).or(Parser.constant(List.empty))

  lazy private val functionStatement = FUNCTION.and(ID.bind(name => {
    LEFT_PAREN.and(paramsStatement).bind(params => {
      RIGHT_PAREN.and(blockStatement(statement)).bind(body => {
        if (name == "main") Parser.constant(Main(body))
        else Parser.constant(Function(name, params, body))
      })
    })
  }))

  statement = functionStatement.or(statement)

  lazy val parser: Parser[Block] = IGNORED.and(Parser.zeroOrMore(statement)).bind(body => {
    Parser.constant(Block(body))
  })

}