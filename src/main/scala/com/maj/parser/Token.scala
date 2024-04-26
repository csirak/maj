package com.maj.parser

import com.maj.ast._
import com.maj.typecheck._


object Token {
  private def regexp(s: String): Parser[String] = new Parser(source => {
    source.check(s.r)
  })

  val whitespaceRegex = "[ \\n\\r\\t]+"
  val singleCommentRegex = "//.*/"
  val multiCommentRegex = "/*.*[*]/"
  private val WHITESPACE = regexp(whitespaceRegex)
  private val COMMENTS = regexp(singleCommentRegex).or(regexp(multiCommentRegex))
  private val IGNORED = WHITESPACE.or(COMMENTS).zeroOrMore

  private def clean(pattern: String) = {
    for {
      _ <- IGNORED
      value <- regexp(pattern)
    } yield value
  }

  // Keywords
  private val FUNCTION = clean("func")
  private val IF = clean("if")
  private val ELSE = clean("else")
  private val RETURN = clean("return")
  private val VAR = clean("var")
  private val WHILE = clean("while")
  private val BOOL_TAG = clean("bool")
  private val INT_TAG = clean("int")
  private val VOID_TAG = clean("void")
  private val STRUCT = clean("struct")
  private val TYPE = clean("type")
  private val ASM = clean("asm")
  private val CONST = clean("const")
  private val LINE = clean("[^}]*")

  // Punctuation
  private val COMMA = clean("[,]")
  private val ASSIGN = clean("=")
  private val SEMICOLON = clean(";")
  private val COLON = clean(":")
  private val LEFT_PAREN = clean("\\(")
  private val RIGHT_PAREN = clean("\\)")
  private val LEFT_CURLY = clean("\\{")
  private val RIGHT_CURLY = clean("\\}")

  // Operators
  private val SINGLE_AND = clean("\\&").map(_ => MajTypeComposeAnd())
  private val SINGLE_OR = clean("\\|").map(_ => MajTypeComposeOr())

  private val NOT = clean("\\!").map(_ => Not())
  private val EQUAL: Parser[AstOperator] = clean("==").map(_ => Equals())
  private val GTOREQ: Parser[AstOperator] = clean(">=").map(_ => GreaterThanOrEquals())
  private val LTOREQ: Parser[AstOperator] = clean("<=").map(_ => LessThanOrEquals())
  private val GREATER: Parser[AstOperator] = clean(">").map(_ => GreaterThan())
  private val LESS: Parser[AstOperator] = clean("<").map(_ => LessThan())
  private val NEQ: Parser[AstOperator] = clean("!=").map(_ => NotEquals())
  private val AND: Parser[AstOperator] = clean("&&").map(_ => And())
  private val OR: Parser[AstOperator] = clean("\\|\\|").map(_ => Or())

  private val ADD: Parser[AstOperator] = clean("\\+").map(_ => Add())
  private val SUB: Parser[AstOperator] = clean("\\-").map(_ => Sub())
  private val MUL: Parser[AstOperator] = clean("\\*").map(_ => Mul())
  private val DIV: Parser[AstOperator] = clean("\\/").map(_ => Div())
  private val MOD: Parser[AstOperator] = clean("\\%").map(_ => Mod())


  // Values
  private val BINARY = clean("0b").and(clean("[01]+")).map(bin => MajInt(Integer.parseInt(bin, 2)))
  private val HEX = clean("0x").and(clean("[0-9a-fA-F]+")).map(hex => MajInt(Integer.parseInt(hex, 16)))
  private val DEC: Parser[ASTNode] = clean("[0-9]+").map(digits => MajInt(digits.toInt))
  private val NUMBER: Parser[ASTNode] = BINARY.or(HEX).or(DEC)
  private val BOOL: Parser[ASTNode] = clean("true|false").map(bool => MajBool(bool.toBoolean))
  private val NULL: Parser[ASTNode] = clean("null").map(_ => MajNull())
  private val ASCII: Parser[ASTNode] = clean("'[ -~]'").map(char => MajChar(char.charAt(1)))

  private val ID = clean("[a-zA-Z_][a-zA-Z0-9_]*")


  // Expressions

  private var typeExpression: Parser[TypeNode] = for {
    tag <- BOOL_TAG.or(INT_TAG).or(VOID_TAG).or(ID)
  } yield MajType(tag)

  private val wrappedTypeExpression = for {
    _ <- LEFT_PAREN
    typ <- typeExpression
    _ <- RIGHT_PAREN
  } yield typ

  typeExpression = wrappedTypeExpression.or(typeExpression)

  private val typeCompose: Parser[TypeNode] = infix(SINGLE_OR.or(SINGLE_AND), typeExpression)

  typeExpression = typeCompose.or(typeExpression)

  private val ANNOTATION = for {
    _ <- COLON
    typ <- typeExpression
  } yield typ


  private val ID_ANNOTATION = for {
    name <- ID
    typ <- ANNOTATION
  } yield (name, typ)

  private val STRUCTDEF = for {
    _ <- STRUCT
    _ <- LEFT_CURLY
    fields <- {
      for {
        field <- ID_ANNOTATION
        _ <- SEMICOLON
      } yield field
    }
    _ <- RIGHT_CURLY
  } yield fields


  private val id: Parser[ASTNode] = ID.map(name => Iden(name))
  private var expression: Parser[ASTNode] = BOOL.or(List(ASCII, id, NUMBER, NULL))

  private def arguments(expression: Parser[ASTNode]): Parser[List[ASTNode]] = for {
    firstArg <- expression.optional[ASTNode]
    otherArgs <- COMMA.and(expression).zeroOrMore
  } yield firstArg.toList ++ otherArgs


  private val call: Parser[ASTNode] = for {
    name <- ID
    _ <- LEFT_PAREN
    args <- arguments(expression).optional[List[ASTNode]]
    _ <- RIGHT_PAREN
  } yield Call(name, args.getOrElse(List.empty))


  expression = call.or(expression)

  private val wrappedExpression = for {
    _ <- LEFT_PAREN
    exp <- expression
    _ <- RIGHT_PAREN
  } yield exp

  expression = wrappedExpression.or(expression)

  private val unary: Parser[ASTNode] = for {
    not <- NOT
    exp <- expression
  } yield not.get(exp)

  expression = unary.or(expression)

  private def infix[T](operator: Parser[Operator[T]], operand: Parser[T]): Parser[T] = {
    val repeatedOps = for {
      op <- operator
      exp <- operand
    } yield (op, exp)

    for {
      first <- operand()
      ops <- repeatedOps.zeroOrMore
    } yield ops.foldLeft(first)((acc, opExp) => {
      val (op, exp) = opExp
      op.get(acc, exp)
    })
  }


  private val product = infix(MUL.or(DIV).or(MOD), expression)
  expression = product.or(expression)
  private val sum = infix(ADD.or(SUB), expression)
  expression = sum.or(expression)
  private val comparison = infix(EQUAL.or(List(NEQ, GTOREQ, LTOREQ, GREATER, LESS, AND, OR)), expression)

  expression = comparison.or(expression)


  // statements
  private val expressionStatement = expression.bind(exp => SEMICOLON.and(Parser.constant(exp)))
  private val returnStatement = for {
    _ <- RETURN
    exp <- expressionStatement
  } yield Return(exp)

  private val returnVoidStatement = for {
    _ <- RETURN
    _ <- SEMICOLON
  } yield Return(MajNull());


  private val typeStatement = for {
    _ <- TYPE
    name <- ID
    typ <- ANNOTATION
    _ <- SEMICOLON
  } yield TypeDef(name, typ)


  private val varStatement: Parser[ASTNode] = for {
    _ <- VAR
    name <- ID
    _ <- ASSIGN
    value <- expressionStatement
  } yield MutableVar(name, value)

  private val constStatement: Parser[ASTNode] = for {
    _ <- CONST
    name <- ID
    _ <- ASSIGN
    value <- expressionStatement
  } yield ConstVar(name, value)

  private var statement: Parser[ASTNode] = returnStatement.or(List(returnVoidStatement, typeStatement, constStatement, varStatement, expressionStatement))

  private val assignStatement = for {
    name <- ID
    _ <- ASSIGN
    value <- expressionStatement
  } yield Assign(name, value)


  statement = assignStatement.or(statement)

  private val blockStatement = (statement: Parser[ASTNode]) => for {
    _ <- LEFT_CURLY
    statements <- statement.zeroOrMore
    _ <- RIGHT_CURLY
  } yield Block(statements)


  statement = blockStatement(statement).or(statement)


  private val elseStatement: Parser[ASTNode] = for {
    _ <- ELSE
    block <- blockStatement(statement)
  } yield block


  lazy private val ifStatement = for {
    _ <- IF.and(LEFT_PAREN)
    cond <- expression
    _ <- RIGHT_PAREN
    block <- blockStatement(statement)
    optElse <- ifElseStatement.optional
  } yield Conditional(cond, block, optElse)

  lazy private val ifElseStatement: Parser[ASTNode] = ELSE.and(ifStatement).or(elseStatement)

  statement = ifStatement.or(statement)

  private val asmStatement = for {
    _ <- ASM
    _ <- LEFT_CURLY
    asm <- LINE
    _ <- RIGHT_CURLY

  } yield AsmBlock(asm.stripMargin.split("\n").toList.map(cleanAsm))

  statement = asmStatement.or(statement)


  private val whileStatement = for {
    _ <- WHILE.and(LEFT_PAREN)
    cond <- expression
    _ <- RIGHT_PAREN
    block <- blockStatement(statement)
  } yield Loop(cond, block)

  statement = whileStatement.or(statement)


  private val paramsStatement: Parser[List[(String, TypeNode)]] = for {
    firstParamOpt <- ID_ANNOTATION.optional[(String, TypeNode)]
    otherParams <- COMMA.and(ID_ANNOTATION).zeroOrMore
  } yield firstParamOpt.toList ++ otherParams

  private val functionStatement = (statement: Parser[ASTNode]) => for {
    _ <- FUNCTION
    name <- ID
    _ <- LEFT_PAREN
    params <- paramsStatement
    _ <- RIGHT_PAREN
    returnType <- ANNOTATION
    body <- blockStatement(statement)
  } yield Function(name, params.map(_._1), MajFuncType(returnType, params.map(_._2)), body)


  statement = functionStatement(statement).or(statement)

  val parser: Parser[Block] = for {
    _ <- IGNORED
    block <- statement.zeroOrMore
    _ <- IGNORED
  } yield Block(block)


  private def cleanAsm(line: String): String = {
    val regex = "\\s*(\\S+)\\s*".r
    val matches = regex.findAllIn(line).matchData.map(_.group(1)).toList
    if (matches.isEmpty) return ""
    val out = matches.head + "\t\t" + matches.tail.mkString(" ")
    if (out.contains(":")) out else "\t\t" + out
  }
}