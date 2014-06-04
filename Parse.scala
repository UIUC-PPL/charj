package CharjParser

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token._
import scala.util.parsing._
import scala.util.parsing.combinator.lexical._
import scala.io._
import scala.language.postfixOps
import scala.util.parsing.input.Positional

object Parse extends StandardTokenParsers with App {
  lexical.reserved += ("class", "entry", "def", "val", "var",
                       "chare", "mainchare", "charearray",
                       "if", "else", "true", "false", "new",
                       "for", "while", "return", "null")
  lexical.delimiters += ("=", "+", "-", "*", "/", "==",
                         "{", "}", "[", "]", "(", ")",
                         ":", ".", ",", ";", "&&", "||", "!",
                         "<", "<=", ">", ">=", "+=", "-=", "#")

  val input = Source.fromFile("../input4.test").getLines.reduceLeft[String](_ + '\n' + _)
  val tokens = new lexical.Scanner(input)

  val result = phrase(program)(tokens)

  result match {
    case Success(tree, _) => {
      import BaseContext.verbose
      verbose = true

      if (verbose) println("--- successfully parsed AST ---")
      if (verbose) println(tree)

      BaseContext.base = tree

      if (verbose) println("--- begin complete symbol collection ---")
      val col = new Collector(tree)
      col.start()
      if (verbose) println("--- symbol collections in nested contexts ---")
      if (verbose) col.print(Tuple2(BaseContext.context, EmptyStmt()), 1)

      val checker = new Checker(tree)
      checker.start()

      println("Static checker finished")
    }
    case e: NoSuccess => {
      Console.err.println(e)
    }
  }

  def program = positioned(outerStmt.* ^^ { case stmts => StmtList(stmts) })

  def outerStmt = positioned(
    classStmt | chareStmt
  )

  def innerStmtList = innerStmt.*

  def innerStmt = positioned(
    defStmt | varStmt <~ ";"
  )

  def empty = ";" ^^^ EmptyStmt()

  def semiStmt : Parser[Stmt] = positioned(
      varStmt <~ ";"
    | empty
    | assignStmt <~ ";"
    | expression <~ ";" ^^ { case expr => ExprStmt(expr) }
    | ifStmt
    | forStmt
    | whileStmt
    | returnStmt <~ ";"
    | "{" ~> semiStmt.* <~ "}"   ^^ { case stmts  => StmtList(stmts) }
  )

  def isSystem =
    "#".? ^^ {
      case Some(x) => true
      case None => false
    }

  def classStmt = positioned(
    "class" ~ isSystem ~ ident ~ genericOpen.? ~ typeStmt.? ~ "{" ~ innerStmtList ~ "}"
    ^^ { case _ ~ isSystem ~ ident ~ generic ~ typeStmt ~ _ ~ stmts ~ _ =>
      ClassStmt(ident, isSystem, if (generic.isEmpty) List() else generic.get, typeStmt, stmts)
    }
  )

  def chareStmt = positioned(
    "chare" ~ ident ~ "{" ~ innerStmtList ~ "}"
    ^^ { case _ ~ ident ~ _ ~ stmts ~ _ => ChareStmt(ident, stmts) }
  )

  def semi = "{" ~ semiStmt.* ~ "}" ^^ { case _ ~ semi ~ _ => StmtList(semi) }

  def defStmt = positioned(
    "entry".? ~ "def" ~ ident ~ "(" ~ mkList(typedParam, ",").? ~ ")" ~ typeStmt.? ~ (semi | empty)
    ^^ { case isEntry ~ _ ~ ident ~ _ ~ typedParamList ~ _ ~ typeStmt ~ semi =>
      DefStmt(isEntry, ident, typedParamList, typeStmt, semi)
    }
  )

  def typedParam = positioned(
    ident ~ typeStmt
    ^^ { case ident ~ typeStmt => TypeParam(ident, typeStmt) }
  )

  def declStart = (
      "var" ^^ { case _ => true }
    | "val" ^^ { case _ => false }
  )

  def equalExpr = "=" ~ expression ^^ { case _ ~ expr => expr }

  def varStmt = positioned(
    declStart ~ ident ~ typeStmt.? ~ equalExpr.?
    ^^ { case mutable ~ ident ~ typeStmt ~ expr => DeclStmt(mutable, ident, typeStmt, expr) }
  )

  def typeStmt = positioned(
    ":" ~ qualifiedIdent ~ generic.?
    ^^ { case _ ~ qualIdent ~ generic => Type(
      if (generic.isEmpty)
        Tok(qualIdent.reduce(_ + _))
      else
        Fun(qualIdent.reduce(_ + _), generic.get)
    ) }
  )

  def ifStmt = positioned(
    "if" ~ "(" ~ expression ~ ")" ~ semiStmt ~ elseStmt.?
    ^^ { case _ ~ _ ~ cond ~ _ ~ expr1 ~ expr2 => IfStmt(cond, expr1, expr2) }
  )

  def forStmt = positioned(
    "for" ~ "(" ~ mkList(varStmt, ",") ~ ";" ~ expression ~ ";" ~ mkList(assignStmt, ",") ~ ")" ~ semiStmt
    ^^ { case _ ~ _ ~ varStmts ~ _ ~ expr1 ~ _ ~ assign ~ _ ~ stmt => ForStmt(varStmts, expr1, assign, stmt) }
  )

  def mkList[T](rule : Parser[T], op : String) =
    rule ~ followMkList(rule,op).* ^^ { case fst ~ rest => fst :: rest }
  def followMkList[T](rule : Parser[T], op : String) = op ~> rule

  def whileStmt = positioned(
    "while" ~ "(" ~  expression ~ ")" ~ semiStmt
    ^^ { case _ ~ _ ~  expr1 ~ _ ~ stmt => WhileStmt(expr1, stmt) }
  )
  def returnStmt = positioned(
    "return" ~ expression.?
    ^^ { case _ ~ exp => ReturnStmt(exp) }
  )
  def assignOp = (
      "="  ^^^ Equal()
    | "+=" ^^^ PEqual()
    | "-=" ^^^ MEqual()
  )
  def assignStmt = positioned(
    qualifiedIdent ~ assignOp ~ expression ^^ { case id ~ op ~ expr => AssignStmt(StrExpr(id), op, expr) }
  )

  def elseStmt = "else" ~> semiStmt

  def genericOpen : Parser[List[Term]] = "[" ~> qualifiedIdentOpen <~ "]"
  def qualifiedIdentOpen = mkList(qualifiedIdent ~ genericOpen.? ^^ { case ident ~ maybeGeneric =>
    if (maybeGeneric.isEmpty)
        MVar(ident.reduce(_ + _))
      else
        Fun(ident.reduce(_ + _), maybeGeneric.get)
   }, ",")

  def generic : Parser[List[Term]] = "[" ~> qualifiedIdentList <~ "]"

  def qualifiedIdentList = mkList(qualifiedIdent ~ generic.? ^^ { case ident ~ maybeGeneric =>
    if (maybeGeneric.isEmpty)
        Tok(ident.reduce(_ + _))
      else
        Fun(ident.reduce(_ + _), maybeGeneric.get)
   }, ",")

  def qualifiedIdent = mkList(ident, ".")

  def expression : Parser[Expression] = bOr

  def funcCall = positioned(
    qualifiedIdent ~ generic.? ~ "(" ~ parameters.? ~ ")"
    ^^ { case ident ~ gen ~ _ ~ params ~ _ => FunExpr(ident, if (gen.isEmpty) List() else gen.get, params) }
  )

  def newExpr = positioned(
    "new" ~ qualifiedIdent ~ generic.? ~ "(" ~ parameters.? ~ ")"
    ^^ { case _ ~ ident ~ generic ~ _ ~ params ~ _ => NewExpr(ident, if (generic.isEmpty) List() else generic.get,
                                                              params) }
  )

  def parameters = mkList(expression, ",")

  def fact : Parser[Expression] = positioned(
      funcCall
    | newExpr
    | "true"                     ^^ { case _      => True() }
    | "false"                    ^^ { case _      => False() }
    | "null"                     ^^ { case _      => Null() }
    |  qualifiedIdent            ^^ { case qident => StrExpr(qident) }
    | "-" ~> expression          ^^ { case expr   => NegExpr(expr) }
    | numericLit                 ^^ { case lit    => NumLiteral(lit) }
    | stringLit                  ^^ { case lit    => StrLiteral(lit) }
    | "(" ~> expression <~ ")"   ^^ { case expr   => expr }
    | "!" ~> expression          ^^ { case expr   => NotExpr(expr) }
  )

  def dot : Parser[Expression] = positioned(
    fact ~ rep("." ~ ident)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "." ~ y) => DotExpr(x, StrLiteral(y))
      }
    }
  )

  def term : Parser[Expression] = positioned(
    dot ~ rep("*" ~ dot | "/" ~ dot)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "*" ~ y) => MulExpr(x, y)
        case (x, "/" ~ y) => DivExpr(x, y)
      }
    }
  )

  def expr : Parser[Expression] = positioned(
    term ~ rep("+" ~ term | "-" ~ term)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "+" ~ y) => AddExpr(x, y)
        case (x, "-" ~ y) => SubExpr(x, y)
      }
    }
  )

  def comp : Parser[Expression] = positioned(
    expr ~ rep("<" ~ expr | "<=" ~ expr | ">" ~ expr | ">=" ~ expr | "==" ~ expr)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "<" ~ y) =>  LesExpr(x, y)
        case (x, "<=" ~ y) => LeqExpr(x, y)
        case (x, ">" ~ y) =>  GesExpr(x, y)
        case (x, ">=" ~ y) => GeqExpr(x, y)
        case (x, "==" ~ y) => ComExpr(x, y)
      }
    }
  )

  def bAnd : Parser[Expression] = positioned(
    comp ~ rep("&&" ~ comp)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "&&" ~ y) => AndExpr(x, y)
      }
    }
  )

  def bOr : Parser[Expression] = positioned(
    bAnd ~ rep("||" ~ bAnd)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "||" ~ y) => OrrExpr(x, y)
      }
    }
  )
}
