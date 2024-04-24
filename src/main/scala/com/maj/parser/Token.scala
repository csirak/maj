package com.maj.parser

import com.maj.ast._


object Token {
  implicit class StringRegexAddition(val s: String) extends AnyVal {
    def regexp: Parser[String] = {
      new Parser(source => {
        source.check(s.r)
      })
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
  private val COLON = clean(":")
  private val LEFT_PAREN: Parser[String] = clean("\\(")
  private val RIGHT_PAREN: Parser[String] = clean("\\)")
  private val LEFT_CURLY = clean("\\{")
  private val RIGHT_CURLY = clean("\\}")

  // TODO: Add hex and bin encoding
  private val BINARY = clean("0b").and(clean("[01]+")).map(bin => MajInt(Integer.parseInt(bin, 2)))
  private val HEX = clean("0x").and(clean("[0-9a-fA-F]+")).map(hex => MajInt(Integer.parseInt(hex, 16)))
  private val DEC: Parser[ASTNode] = clean("[0-9]+").map(digits => MajInt(digits.toInt))
  private val NUMBER: Parser[ASTNode] = BINARY.or(HEX).or(DEC)
  private val BOOL: Parser[ASTNode] = clean("true|false").map(bool => MajBool(bool.toBoolean))
  private val NULL: Parser[ASTNode] = clean("null").map(_ => MajNull())

  private val ID = clean("[a-zA-Z_][a-zA-Z0-9_]*")

  private val BOOLTAG = clean("bool")
  private val INTTAG = clean("int")
  private val VOIDTAG = clean("void")
  private val STRUCT = clean("struct")

  private var TYPE = BOOLTAG.or(INTTAG).or(VOIDTAG).or(ID)

  private val TYPEDEF = clean("type")

  private val ANNOTATION = COLON.and(TYPE.bind(typ => {
    Parser.constant(typ)
  }))

  private val IDANNOTATION = ID.bind(name => {
    ANNOTATION.bind(typ => {
      Parser.constant((name, typ))
    })
  })


  private val STRUCTDEF = STRUCT.and(LEFT_CURLY.bind(_ => Parser.zeroOrMore(IDANNOTATION).bind(fields => {
    RIGHT_CURLY.and(ID.bind(name => {
      SEMICOLON.and(Parser.constant((name, fields)))
    }))
  })))

  private val NOT: Parser[Not] = clean("\\!").map(_ => Not())

  private val EQUAL: Parser[Operator] = clean("==").map(_ => Equals())
  private val GTOREQ: Parser[Operator] = clean(">=").map(_ => GreaterThanOrEquals())
  private val LTOREQ: Parser[Operator] = clean("<=").map(_ => LessThanOrEquals())
  private val GREATER: Parser[Operator] = clean(">").map(_ => GreaterThan())
  private val LESS: Parser[Operator] = clean("<").map(_ => LessThan())
  private val NEQ: Parser[Operator] = clean("!=").map(_ => NotEquals())
  private val AND: Parser[Operator] = clean("&&").map(_ => And())
  private val OR: Parser[Operator] = clean("\\|\\|").map(_ => Or())

  private val ADD: Parser[Operator] = clean("\\+").map(_ => Add())
  private val SUB: Parser[Operator] = clean("\\-").map(_ => Sub())
  private val MUL: Parser[Operator] = clean("\\*").map(_ => Mul())
  private val DIV: Parser[Operator] = clean("\\/").map(_ => Div())
  private val MOD: Parser[Operator] = clean("\\%").map(_ => Mod())

  private val id: Parser[ASTNode] = ID.map(name => Iden(name))
  private var expression: Parser[ASTNode] = BOOL.or(id).or(NUMBER).or(NULL)

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
  private val comparison = infix(EQUAL.or(NEQ).or(GTOREQ.or(LTOREQ).or(GREATER).or(LESS).or(AND).or(OR)), expression)

  expression = comparison.or(expression)


  lazy private val expressionStatement = expression.bind(exp => SEMICOLON.and(Parser.constant(exp)))
  lazy private val returnStatement = RETURN.and(expressionStatement).bind(exp => Parser.constant(Return(exp)))


  lazy private val varStatement: Parser[ASTNode] = VAR.bind(_ => ID.bind(name => {
    ASSIGN.and(expression.bind(value => SEMICOLON.and(Parser.constant(Create(name, value)))))
  }))

  private var statement: Parser[ASTNode] = returnStatement.or(varStatement).or(expressionStatement)

  lazy private val assignStatement = ID.bind(name => {
    ASSIGN.and(expression).bind(value => SEMICOLON.and(Parser.constant(Assign(name, value))))
  })
  statement = assignStatement.or(statement)


  lazy private val blockStatement = (statement: Parser[ASTNode]) => LEFT_CURLY.and(Parser.zeroOrMore(statement).bind(body => {
    RIGHT_CURLY.and(Parser.constant(Block(body)))
  }))

  statement = blockStatement(statement).or(statement)


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
      ifElseStatement.bind(statement => {
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

  lazy private val paramsStatement: Parser[List[(String, String)]] = IDANNOTATION.bind(param => {
    Parser.zeroOrMore(COMMA.and(IDANNOTATION)).bind(params => {
      Parser.constant(param :: params)
    })
  }).or(Parser.constant(List.empty))

  lazy private val functionStatement = (statement: Parser[ASTNode]) => FUNCTION.and(ID.bind(name => {
    LEFT_PAREN.and(paramsStatement).bind(params => {
      RIGHT_PAREN.and(ANNOTATION.bind(returnType => {
        blockStatement(statement).bind(body => {

          Parser.constant(Function(name, params.map(_._1), MajFuncType(returnType, params.map(_._2)), body))
        })
      }))
    })
  }))

  statement = functionStatement(statement).or(statement)

  lazy val parser: Parser[Block] = IGNORED.and(Parser.zeroOrMore(statement)).bind(body => {
    Parser.constant(Block(body))
  })

}