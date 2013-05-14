package CharjParser

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token._
import scala.util.parsing._
import scala.util.parsing.combinator.lexical._
import scala.io._
import scala.language.postfixOps

object Parse extends StandardTokenParsers with App {
  lexical.reserved += ("class", "entry", "def", "val", "var",
                       "chare", "mainchare", "charearray",
                       "if", "else", "true", "false", "new")
  lexical.delimiters += ("=", "+", "-", "*", "/", "==",
                         "{", "}", "[", "]", "(", ")",
                         ":", ".", ",", ";", "&&", "||", "!")
 
  val input = Source.fromFile("../input.test").getLines.reduceLeft[String](_ + '\n' + _)
  val tokens = new lexical.Scanner(input)
 
  val result = phrase(program)(tokens)
 
  result match {
    case Success(tree, _) => println(tree)
    case e: NoSuccess => {
      Console.err.println(e)
    }
  }

  def program = outerStmt.* ^^ { case stmts => StmtList(stmts) }
  
  def outerStmt = (
    classStmt | chareStmt
  )

  def innerStmtList = innerStmt.*

  def innerStmt = (
    defStmt | varStmt <~ ";"
  )

  def semiStmt : Parser[Stmt] = (
      varStmt <~ ";"
    | assignStmt <~ ";"
    | expression <~ ";" ^^ { case expr => ExprStmt(expr) }
    | ifStmt
    | "{" ~> semiStmt.* <~ "}"   ^^ { case stmts  => StmtList(stmts) }
  )

  def classStmt = (
    "class" ~ ident ~ "{" ~ innerStmtList ~ "}"
    ^^ { case _ ~ ident ~ _ ~ stmts ~ _ => ClassStmt(ident, stmts) }
  )

  def chareStmt = (
    "chare" ~ ident ~ "{" ~ innerStmtList ~ "}"
    ^^ { case _ ~ ident ~ _ ~ stmts ~ _ => ChareStmt(ident, stmts) }
  )

  def defStmt = (
    "entry".? ~ "def" ~ ident ~ "(" ~ typedParamList.? ~ ")" ~ typeStmt.? ~ "{" ~ semiStmt.* ~ "}"
    ^^ { case isEntry ~ _ ~ ident ~ _ ~ typedParamList ~ _ ~ typeStmt ~ _ ~ semiStmt ~ _ =>
      DefStmt(isEntry, ident, typedParamList, typeStmt, semiStmt)
    }
  )

  def typedParamList = (
    typedParam ~ followTypedParam.*
    ^^ {
      case param ~ rest => param :: rest
    }
  )

  def followTypedParam = "," ~> typedParam

  def typedParam = (
    ident ~ typeStmt
    ^^ { case ident ~ typeStmt => TypeParam(ident, typeStmt) }
  )

  def declStart = (
      "var" ^^ { case _ => true }
    | "val" ^^ { case _ => false }
  )

  def varStmt = (
    declStart ~ ident ~ typeStmt ~ "=" ~ expression
    ^^ { case mutable ~ ident ~ typeStmt ~ _ ~ expr => DeclStmt(mutable, ident, typeStmt, expr) }
  )

  def typeStmt = (
    ":" ~ qualifiedIdent ~ generic.?
    ^^ { case _ ~ qualIdent ~ generic => Type(qualIdent, generic) }
  )

  def ifStmt = (
    "if" ~ "(" ~ expression ~ ")" ~ semiStmt ~ elseStmt.?
    ^^ { case _ ~ _ ~ cond ~ _ ~ expr1 ~ expr2 => IfStmt(cond, expr1, expr2) }
  )

  def assignStmt = qualifiedIdent ~ "=" ~ expression ^^ { case ident ~ _ ~ expr => AssignStmt(ident, expr) }
    
  def elseStmt = "else" ~> semiStmt

  def generic : Parser[List[Type]] = "[" ~> qualifiedIdentList <~ "]"

  def qualifiedIdentList = (
    qualifiedIdent ~ generic.? ~ followQualifiedIdent.*
    ^^ { case ident ~ maybeGeneric ~ rest => Type(ident, maybeGeneric) :: rest }
  )
  def followQualifiedIdent = (
    "," ~ qualifiedIdent ~ generic.?
    ^^ { case _ ~ ident ~ maybeGeneric => Type(ident, maybeGeneric) }
  )

  def qualifiedIdent = (
    ident ~ followIdent.*
    ^^ { case param ~ rest => param :: rest }
  )
  def followIdent = "." ~> ident

  def expression : Parser[Expression] = bComp

  def funcCall = (
    qualifiedIdent ~ "(" ~ parameters.? ~ ")"
    ^^ { case ident ~ _ ~ params ~ _ => FunExpr(ident, params) }
  )

  def newExpr = (
    "new" ~ qualifiedIdent ~ generic.? ~ "(" ~ parameters.? ~ ")"
    ^^ { case _ ~ ident ~ generic ~ _ ~ params ~ _ => NewExpr(ident, generic, params) }
  )

  def parameters = (
    expression ~ followParam.*
    ^^ { case param ~ rest => param :: rest }
  )
  def followParam = "," ~> expression

  def fact : Parser[Expression] = (
      funcCall
    | newExpr
    | "true"                     ^^ { case _      => True() }
    | "false"                    ^^ { case _      => False() }
    |  qualifiedIdent            ^^ { case qident => StrExpr(qident) }
    | "-" ~> expression          ^^ { case expr   => NegExpr(expr) }
    | numericLit                 ^^ { case lit    => NumLiteral(lit) }
    | stringLit                  ^^ { case lit    => StrLiteral(lit) }
    | "(" ~> expression <~ ")"   ^^ { case expr   => expr }
    | "!" ~> expression          ^^ { case expr   => NotExpr(expr) }
  )

  def bAnd : Parser[Expression] = (
    fact ~ rep("&&" ~ fact)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "&&" ~ y) => AndExpr(x, y)
      }
    }
  )

  def bOr : Parser[Expression] = (
    bAnd ~ rep("||" ~ bAnd)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "||" ~ y) => OrrExpr(x, y)
      }
    }
  )

  def term : Parser[Expression] = (
    bOr ~ rep("*" ~ bOr | "/" ~ bOr)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "*" ~ y) => MulExpr(x, y)
        case (x, "/" ~ y) => DivExpr(x, y)
      }
    }
  )

  def expr : Parser[Expression] = (
    term ~ rep("+" ~ term | "-" ~ term)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "+" ~ y) => AddExpr(x, y)
        case (x, "-" ~ y) => SubExpr(x, y)
      }
    }
  )

  def bComp : Parser[Expression] = (
    expr ~ rep("==" ~ expr)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "==" ~ y) => ComExpr(x, y)
      }
    }
  )


}
