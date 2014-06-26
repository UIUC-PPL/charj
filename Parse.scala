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
  override val lexical = new Lexer

  lexical.reserved += ("class", "def", "val", "var",
                       "async", "sync", "wait", "where",
                       "if", "else", "true", "false", "new",
                       "for", "while", "return", "null", "include")
  lexical.delimiters += ("=", "+", "-", "*", "/", "==",
                         "{", "}", "[", "]", "(", ")", "$", "@", "%",
                         ":", ".", ",", ";", "&&", "||", "!", "^", "?",
                         "<", "<=", ">", ">=", "+=", "-=", "#", "<>",
                         "->", "=>")

  // set verbosity to true for testing
  import BaseContext.verbose
  verbose = true

  // assume flattened file names here
  val lstIncludes : ListBuffer[String] = ListBuffer[String]()

  // recursively follow includes
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
            case IncludeStmt(str) => {
              if (!(lstIncludes contains str)) {
                if (verbose) println("including file: " + str)
                curList += str
                lstIncludes += str
              }
            }
            case _ => ;
          }
        }
        val lb : ListBuffer[String] = ListBuffer[String]()
        traverseTree(tree, lb)
        if (lb.length == 0) tree
        else StmtList(lb.map{parseRecur(_)}.map{_.asInstanceOf[StmtList].lst}.reduceLeft(_ ++ _) ++
                      tree.asInstanceOf[StmtList].lst)
      }
      case e: NoSuccess => { Console.err.println(e); System.exit(1); StmtList(List()) }
    }
  }
  def frontEnd(tree : Stmt) {
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
    if (verbose) println("--- front end complete ---")
  }

  def backEnd(tree : Stmt) {
    import java.io._
    if (verbose) println("--- starting back end generation ---")
    val writer = new PrintWriter(new File("out.cc"))
    writer.write("#include <stdio.h>\n")
    writer.write("#include <stdlib.h>\n")
    def printer(s : String) : Unit = {
      //print(s)
      writer.write(s)
    }
    val gen = new CodeGen(tree, printer);
    gen.start()
    writer.close()
    if (verbose) println("--- generation complete ---")
  }

  // run the front end of the compiler
  lstIncludes += args(0)
  frontEnd(parseRecur(args(0)))

  // run the back end for code generationp
  backEnd(BaseContext.base)

  def program = positioned(outerStmt.* ^^ { case stmts => StmtList(stmts) })

  def outerStmt = positioned(
      classStmt
    | defStmt
    | includeStmt
    | failure("only classes, defs, and includes are allowed at the top level")
  )

  def innerStmtList = innerStmt.*

  def innerStmt = positioned(
      defStmt
    | varStmt <~ ";"
    | failure("only defs, vals and vars are allowed inside a class")
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
    | waitStmt
    | returnStmt <~ ";"
    | "{" ~> semiStmt.* <~ "}"   ^^ { case stmts  => StmtList(stmts) }
  )

  def funs : Parser[List[DefStmt]] = "def" ~> funList

  def funList = repsep(funAtom, ",")

  def funAtom = positioned(
    funName ~ generic.?  ~ "(" ~ repsep(typedParam, ",").? ~ ")"
    ^^ { case ident ~ gen ~ _ ~ typedParamList ~ _ =>
      DefStmt(ident, if (gen.isEmpty) List() else gen.get, typedParamList, None, EmptyStmt())
    }
  )

  def whereClause : Parser[Expression] = "where" ~> expression

  def waitStmt : Parser[Stmt] = positioned(
    "wait" ~ funs ~ whereClause.? ~ semiStmt
    ^^ { case _ ~ funs ~ where ~ semi => WaitStmt(funs, where, semi) }
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

  def semi = "{" ~ semiStmt.* ~ "}" ^^ { case _ ~ semi ~ _ => StmtList(semi) }

  def funName = (
      ident
    | "[" ~ "]" ^^ { case _ ~ _ => "[]" }
    | "<" ~ ">" ^^ { case _ ~ _ => "<>" }
    | "#" | "^" | "?"
    | failure("invalid function name: allowed special chars #,^,?,[], or alpha[alpha/numeric]*")
  )

  def includeStmt = positioned(
    "include" ~ stringLit ~ ";" ^^ { case _ ~ str ~ _ => IncludeStmt(str) }
  )

  def defStmt = positioned(
    "def" ~ funName ~ generic.?  ~ "(" ~ repsep(typedParam, ",").? ~ ")" ~ typeStmt.? ~ (semi | empty)
    ^^ { case _ ~ ident ~ gen ~ _ ~ typedParamList ~ _ ~ typeStmt ~ semi =>
      DefStmt(ident, if (gen.isEmpty) List() else gen.get, typedParamList, typeStmt, semi)
    })

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

  def typeAtom = positioned(
    ident ~ generic.?
    ^^ { case ident ~ generic =>
      if (generic.isEmpty)
        Tok(ident)
      else
        Fun(ident, generic.get)
    }
  )

  def optTypeAtom : Parser[Term] = typeAtom | "(" ~> typeAtomListP <~ ")"

  def typeStmt : Parser[Type] = positioned(
    ":" ~> typeAtomListP ^^ { case atom => Type(atom) }
  )

  def typeAtomListP : Parser[Term] =
    typeAtomList ^^ { case lst => if (lst.length == 1) lst.head else Thunker(lst) }
  def typeAtomList : Parser[List[Term]] = repsep(optTypeAtom, "->")

  def ifStmt = positioned(
    "if" ~ "(" ~ expression ~ ")" ~ semiStmt ~ elseStmt.?
    ^^ { case _ ~ _ ~ cond ~ _ ~ expr1 ~ expr2 => IfStmt(cond, expr1, expr2) }
  )

  def forStmt = positioned(
    "for" ~ "(" ~ repsep(varStmt, ",") ~ ";" ~ expression.? ~ ";" ~ repsep(assignStmt, ",") ~ ")" ~ semiStmt
    ^^ { case _ ~ _ ~ varStmts ~ _ ~ expr1 ~ _ ~ assign ~ _ ~ stmt => ForStmt(varStmts, expr1, assign, stmt) }
  )

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
    expression ~ assignOp ~ expression ^^ { case expr1 ~ op ~ expr2 => AssignStmt(expr1, op, expr2) }
  )

  def elseStmt = "else" ~> semiStmt

  def genericOpen : Parser[List[Term]] = "[" ~> qualifiedIdentOpen <~ "]"
  def qualifiedIdentOpen = repsep(ident ~ genericOpen.? ^^ { case ident ~ maybeGeneric =>
    if (maybeGeneric.isEmpty)
        MVar(ident)
      else
        Fun(ident, maybeGeneric.get)
   }, ",")

  def generic : Parser[List[Term]] = "[" ~> qualifiedIdentList <~ "]"

  def qualifiedIdentList = repsep(ident ~ generic.? ^^ { case ident ~ maybeGeneric =>
    if (maybeGeneric.isEmpty)
        Tok(ident)
      else
        Fun(ident, maybeGeneric.get)
   }, ",")

  //def qualifiedIdent = repsep(ident, ".")

  def expression : Parser[Expression] = bOr

  def funcCall = positioned(
    ident ~ generic.? ~ "(" ~ parameters.? ~ ")"
    ^^ { case ident ~ gen ~ _ ~ params ~ _ => FunExpr(ident, if (gen.isEmpty) List() else gen.get, params) }
  )

  def parameters = repsep(expression, ",")

  def anonFunc = positioned(
    "{" ~ "(" ~ repsep(typedParam, ",").? ~ ")" ~ typeStmt.? ~ "=>" ~ semiStmt.* ~  "}" ^^ {
      case _ ~ _ ~ typedParamList ~ _ ~ typeStmt ~ _ ~ semi ~ _ =>
        DefStmt("@afun", List(), typedParamList, typeStmt, StmtList(semi))
    }
  )

  def mainExpr : Parser[Expression] = positioned(
      funcCall
    | "async" ~> expression      ^^ { case expr   => AsyncExpr(expr) }
    | "sync"  ~> expression      ^^ { case expr   => SyncExpr(expr) }
    | anonFunc                   ^^ { case expr   => DefExpr(expr) }
    | "new"   ~> expression      ^^ { case expr   => NewExpr(expr) }
    | "true"                     ^^ { case _      => True() }
    | "false"                    ^^ { case _      => False() }
    | "null"                     ^^ { case _      => Null() }
    | ident                      ^^ { case ident  => StrExpr(ident) }
    | "-" ~> expression          ^^ { case expr   => NegExpr(expr) }
    | numericLit                 ^^ { case lit    => NumLiteral(lit) }
    | stringLit                  ^^ { case lit    => StrLiteral(lit) }
    | "(" ~> expression <~ ")"   ^^ { case expr   => expr }
    | "!" ~> expression          ^^ { case expr   => NotExpr(expr) }
    | failure("illegal start of simple expression")
  )

  def uniOps = "#" | "^" | "?"

  def uniFun : Parser[Expression] = positioned(
    uniOps.? ~ mainExpr
    ^^ { case op ~ ex  => if (op.isEmpty) ex else FunExpr(op.get, List(), Some(List(ex))) }
  )

  def uniCall : Parser[Expression] = positioned(
    uniFun ~ uniOps.?
    ^^ { case ex ~ op  => if (op.isEmpty) ex else DotExpr(ex, FunExpr(op.get, List(), None)) }
  )

  def opCall1 = positioned(
    "[" ~ parameters.? ~ "]" ^^ { case _ ~ params ~ _ => FunExpr("[]", List(), params) }
  )

  def opCall : Parser[Expression] = positioned(
    uniCall ~ rep(opCall1)
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

  def possOps = "$" | "@"
  def possOps2 = "$" | "#" | "@" | "%" | "~" | "+" | "-" | "*" | "/" | "\\"
  def opList : Parser[String] = possOps | possOps ~ opList2 ^^ { case p ~ l => p + l }
  def opList2 : Parser[String] = possOps2 | possOps2 ~ opList2 ^^ { case p ~ l => p + l }
  def opExpr : Parser[Expression] = positioned(
    dot ~ rep(opList ~ dot)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, op ~ y) => AopExpr(x, y, op)
      }
    }
  )

  def term : Parser[Expression] = positioned(
    opExpr ~ rep("*" ~ opExpr | "/" ~ opExpr | "%" ~ opExpr)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "*" ~ y) => MulExpr(x, y)
        case (x, "/" ~ y) => DivExpr(x, y)
        case (x, "%" ~ y) => ModExpr(x, y)
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
    expr ~ rep("<>" ~ expr | "<" ~ expr | "<=" ~ expr | ">" ~ expr | ">=" ~ expr | "==" ~ expr)
    ^^ {
      case el ~ rest => (el /: rest) {
        case (x, "<" ~ y) =>  LesExpr(x, y)
        case (x, "<=" ~ y) => LeqExpr(x, y)
        case (x, ">" ~ y) =>  GesExpr(x, y)
        case (x, ">=" ~ y) => GeqExpr(x, y)
        case (x, "==" ~ y) => ComExpr(x, y)
        case (x, "<>" ~ y) => NeqExpr(x, y)
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
