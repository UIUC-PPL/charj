package CharjParser

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token._
import scala.util.parsing._
import scala.util.parsing.combinator.lexical._
import scala.io._
import scala.language.postfixOps
import scala.util.parsing.input.Positional
import scala.collection.mutable.ListBuffer

object Parse extends StandardTokenParsers with App {
  lexical.reserved += ("class", "entry", "def", "val", "var",
                       "chare", "mainchare", "charearray",
                       "if", "else", "true", "false", "new",
                       "for", "while", "return", "null", "include")
  lexical.delimiters += ("=", "+", "-", "*", "/", "==",
                         "{", "}", "[", "]", "(", ")", "$", "@", "%",
                         ":", ".", ",", ";", "&&", "||", "!", "^", "?",
                         "<", "<=", ">", ">=", "+=", "-=", "#")

  // recursively follow includes (circular includes will cause a problem)
  def parseRecur(file : String) : Stmt = {
    val input = Source.fromFile(file).getLines.reduceLeft[String](_ + '\n' + _)
    val tokens = new lexical.Scanner(input)
    val result = phrase(program)(tokens)
    
    result match {
      case Success(tree, _) => {
        def traverseTree(cur : Stmt, curList : ListBuffer[String]) {
          cur match {
            case StmtList(lst) => {
              for (stmt <- lst) traverseTree(stmt, curList)
            }
            case IncludeStmt(str) => curList += str
            case _ => ;
          }
        }
        val lb : ListBuffer[String] = ListBuffer[String]()
        traverseTree(tree, lb)
        if (lb.length == 0) tree
        else StmtList(lb.map{parseRecur(_)}.map{_.asInstanceOf[StmtList].lst}.reduceLeft(_ ++ _) ++
                      tree.asInstanceOf[StmtList].lst)
      }
      case e: NoSuccess => { Console.err.println(e); StmtList(List()) }
    }
  }
  def frontEnd(tree : Stmt) {
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

  // run the front end of the compiler
  frontEnd(parseRecur(args(0)))

  def program = positioned(outerStmt.* ^^ { case stmts => StmtList(stmts) })

  def outerStmt = positioned(
      classStmt
    | chareStmt
    | defStmt
    | includeStmt
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

  def funName = (
      ident
    | "[" ~ "]" ^^ { case _ ~ _ => "[]" }
    | "<" ~ ">" ^^ { case _ ~ _ => "<>" }
    | "#" | "^" | "?"
  )

  def includeStmt = positioned(
    "include" ~ stringLit ~ ";" ^^ { case _ ~ str ~ _ => IncludeStmt(str) }
  )

  def defStmt = positioned(
    "entry".? ~ "def" ~ funName ~ generic.?  ~ "(" ~ mkList(typedParam, ",").? ~ ")" ~ typeStmt.? ~ (semi | empty)
    ^^ { case isEntry ~ _ ~ ident ~ gen ~ _ ~ typedParamList ~ _ ~ typeStmt ~ semi =>
      DefStmt(isEntry, ident, if (gen.isEmpty) List() else gen.get, typedParamList, typeStmt, semi)
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
    ident ~ generic.? ~ "(" ~ parameters.? ~ ")"
    ^^ { case ident ~ gen ~ _ ~ params ~ _ => FunExpr(List(ident), if (gen.isEmpty) List() else gen.get, params) }
  )

  def newExpr = positioned(
    "new" ~ qualifiedIdent ~ generic.? ~ "(" ~ parameters.? ~ ")"
    ^^ { case _ ~ ident ~ generic ~ _ ~ params ~ _ => NewExpr(ident, if (generic.isEmpty) List() else generic.get,
                                                              params) }
  )

  def parameters = mkList(expression, ",")

  def mainExpr : Parser[Expression] = positioned(
      funcCall
    | newExpr
    | "true"                     ^^ { case _      => True() }
    | "false"                    ^^ { case _      => False() }
    | "null"                     ^^ { case _      => Null() }
    |  ident                     ^^ { case ident  => StrExpr(List(ident)) }
    | "-" ~> expression          ^^ { case expr   => NegExpr(expr) }
    | numericLit                 ^^ { case lit    => NumLiteral(lit) }
    | stringLit                  ^^ { case lit    => StrLiteral(lit) }
    | "(" ~> expression <~ ")"   ^^ { case expr   => expr }
    | "!" ~> expression          ^^ { case expr   => NotExpr(expr) }
  )

  def opCall1 = positioned(
    "[" ~ parameters.? ~ "]" ^^ { case _ ~ params ~ _ => FunExpr(List("[]"), List(), params) }
  )
  def opCall : Parser[Expression] = positioned(
    mainExpr ~ rep(opCall1)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, op) => DotExpr(x, op)
      }
    }
  )

  def dot : Parser[Expression] = positioned(
    opCall ~ rep("." ~ opCall)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "." ~ y) => DotExpr(x, y)
      }
    }
  )

  def uniOps = "#" | "^" | "?"

  def unaryExprPost : Parser[Expression] = positioned(
    uniOps.? ~ dot ^^ {case Some(op) ~ x => FunExpr(List(op), List(), Some(List(x)))
                       case _ ~ x => x}
  )

  def unaryExpr : Parser[Expression] = positioned(
    unaryExprPost ~ rep(uniOps)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, op) => DotExpr(x, FunExpr(List(op), List(), None))
      }
    }
  )

  def possOps = "$" | "@" | "%"
  def possOps2 = "$" | "#" | "@" | "%" | "~" | "+" | "-" | "*" | "/" | "\\"
  def opList : Parser[String] = possOps | possOps ~ opList2 ^^ { case p ~ l => p + l }
  def opList2 : Parser[String] = possOps2 | possOps2 ~ opList2 ^^ { case p ~ l => p + l }
  def opExpr : Parser[Expression] = positioned(
    unaryExpr ~ rep(opList ~ unaryExpr)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, op ~ y) => AopExpr(x, y, op)
      }
    }
  )

  def term : Parser[Expression] = positioned(
    opExpr ~ rep("*" ~ opExpr | "/" ~ opExpr)
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
