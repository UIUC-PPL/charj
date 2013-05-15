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
                       "if", "else", "true", "false", "new",
                       "for", "while", "return")
  lexical.delimiters += ("=", "+", "-", "*", "/", "==",
                         "{", "}", "[", "]", "(", ")",
                         ":", ".", ",", ";", "&&", "||", "!",
                         "<", "<=", ">", ">=", "+=", "-=")

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
    | ";" ^^^ EmptyStmt()
    | assignStmt <~ ";"
    | expression <~ ";" ^^ { case expr => ExprStmt(expr) }
    | ifStmt
    | forStmt
    | whileStmt
    | returnStmt <~ ";"
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
    "entry".? ~ "def" ~ ident ~ "(" ~ mkList(typedParam, ",").? ~ ")" ~ typeStmt.? ~ "{" ~ semiStmt.* ~ "}"
    ^^ { case isEntry ~ _ ~ ident ~ _ ~ typedParamList ~ _ ~ typeStmt ~ _ ~ semiStmt ~ _ =>
      DefStmt(isEntry, ident, typedParamList, typeStmt, semiStmt)
    }
  )

  def typedParam = (
    ident ~ typeStmt
    ^^ { case ident ~ typeStmt => TypeParam(ident, typeStmt) }
  )

  def declStart = (
      "var" ^^ { case _ => true }
    | "val" ^^ { case _ => false }
  )

  def varStmt = (
    declStart ~ ident ~ typeStmt.? ~ "=" ~ expression
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

  def forStmt = (
    "for" ~ "(" ~ mkList(varStmt, ",") ~ ";" ~ expression ~ ";" ~ mkList(assignStmt, ",") ~ ")" ~ semiStmt
    ^^ { case _ ~ _ ~ varStmts ~ _ ~ expr1 ~ _ ~ assign ~ _ ~ stmt => ForStmt(varStmts, expr1, assign, stmt) }
  )

  def mkList[T](rule : Parser[T], op : String) =
    rule ~ followMkList(rule,op).* ^^ { case fst ~ rest => fst :: rest }
  def followMkList[T](rule : Parser[T], op : String) = op ~> rule

  def whileStmt = (
    "while" ~ "(" ~  expression ~ ")" ~ semiStmt
    ^^ { case _ ~ _ ~  expr1 ~ _ ~ stmt => WhileStmt(expr1, stmt) }
  )
  def returnStmt = (
    "return" ~ expression.?
    ^^ { case _ ~ exp => ReturnStmt(exp) }
  )
  def assignOp = (
      "="  ^^^ Equal()
    | "+=" ^^^ PEqual()
    | "-=" ^^^ MEqual()
  )
  def assignStmt = qualifiedIdent ~ assignOp ~ expression ^^ { case id ~ op ~ expr => AssignStmt(id, op, expr) }

  def elseStmt = "else" ~> semiStmt

  def generic : Parser[List[Type]] = "[" ~> qualifiedIdentList <~ "]"

  def qualifiedIdentList = mkList(qualifiedIdent ~ generic.? ^^ { case ident ~ maybeGeneric => Type(ident, maybeGeneric) }, ",")

  def qualifiedIdent = mkList(ident, ".")

  def expression : Parser[Expression] = equal

  def funcCall = (
    qualifiedIdent ~ "(" ~ parameters.? ~ ")"
    ^^ { case ident ~ _ ~ params ~ _ => FunExpr(ident, params) }
  )

  def newExpr = (
    "new" ~ qualifiedIdent ~ generic.? ~ "(" ~ parameters.? ~ ")"
    ^^ { case _ ~ ident ~ generic ~ _ ~ params ~ _ => NewExpr(ident, generic, params) }
  )

  def parameters = mkList(expression, ",")

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

  def comp : Parser[Expression] = (
    fact ~ rep("<" ~ fact | "<=" ~ fact | ">" ~ fact | ">=" ~ fact)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "<" ~ y) =>  LesExpr(x, y)
        case (x, "<=" ~ y) => LeqExpr(x, y)
        case (x, ">" ~ y) =>  GesExpr(x, y)
        case (x, ">=" ~ y) => GeqExpr(x, y)
      }
    }
  )

  def bAnd : Parser[Expression] = (
    comp ~ rep("&&" ~ comp)
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

  def equal : Parser[Expression] = (
    expr ~ rep("==" ~ expr)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "==" ~ y) => ComExpr(x, y)
      }
    }
  )
}
