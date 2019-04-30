package FrontEnd

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token._
import scala.util.parsing._
import scala.util.parsing.combinator.lexical._
import scala.io._
import java.io.File
import scala.language.postfixOps
import scala.util.parsing.input.Positional
import scala.collection.mutable.ListBuffer

object Parse {
  var verbose = false

  def createFunCallID(id : String, expr : List[AstTraversable]) : FuncCall =
    FuncCall(Type(Identifier(List(id)), List(), None), expr)
}

class Parse extends StandardTokenParsers {
  import Parse.{verbose,createFunCallID}

  override val lexical = new Lexer

  lexical.reserved += ("class", "def", "val", "var",
                       "async", "sync", "wait", "where",
                       "if", "else", "true", "false", "new", "delete", "null",
                       "for", "while", "return", "null", "include",
                       "scope", "override", "abstract", "import", "this",
                       "chare", "entry", "system", "super", "do", "trait",
                       "template", "proxy", "thisProxy", "mainchare",
                       "array1d", "array2d", "array3d", "thisIndex", "type",
                       "reducer", "when"
                      )
  lexical.delimiters += ("=", "+", "-", "*", "/", "==",
                         "{", "}", "[", "]", "(", ")", "$", "@", "%",
                         ":", ".", ",", ";", "&&", "||", "!", "^", "?",
                         "<", "<=", ">", ">=", "+=", "-=", "#", "!=",
                         "->", "=>", "::", ":>", "_", "++", "--", "[]",
                         "<>", "~")

  // assume flattened file names here
  val lstIncludes : ListBuffer[String] = ListBuffer[String]()

  def parseIfFound(f : File, str : String, paths : Seq[String]) : Option[AstTraversable] = {
    if (f.exists)
      Some(parseRecurFound(Source.fromFile(str).getLines.reduceLeft[String](_ + '\n' + _), paths))
    else
      None
  }

  def parseRecur(file : String, paths : Seq[String]) : AstTraversable = {
    val lst : List[String] = List(file, "base/" + file) ++ paths.map(_ + "/" + file)

    for (fstr <- lst) {
      val f = new File(fstr)
      val ret = parseIfFound(f, fstr, paths)
      if (!ret.isEmpty) return ret.get
    }

    println("file could not be found: " + file);
    System.exit(1)
    return Namespace("", List())
  }

  // recursively follow includes
  def parseRecurFound(input : String, paths : Seq[String]) : AstTraversable = {
    val tokens = new lexical.Scanner(input)
    val result = phrase(program)(tokens)

    def findInclude(x : AstTraversable, lb : ListBuffer[String]) : Unit = {
      x match {
        case IncludeAst(str) => {
          if (!(lstIncludes contains str)) {
            if (verbose) println("including file: " + str)
            lb += str
            lstIncludes += str
          }
        }
        case _ => ;
      }
    }

    result match {
      case Success(tree, _) => {
        val lb : ListBuffer[String] = ListBuffer[String]()
        tree.traverse[ListBuffer[String]](findInclude, lb, null)
        if (lb.length == 0) tree
        else Namespace("", lb.map{parseRecur(_, paths)}.map{_.asInstanceOf[Namespace].lst}.reduceLeft(_ ++ _) ++
          tree.asInstanceOf[Namespace].lst)
      }
      case e: NoSuccess => { Console.err.println(e); System.exit(1); Namespace("", List()) }
    }
  }

  def program = positioned(outerStmt.* ^^ { case stmts => Namespace("", stmts) })

  def outerStmt : Parser[AstTraversable] = positioned(
      classStmt
    | traitStmt
    | defStmt
    | declStmt
    | includeStmt
    | namespaceStmt
    | importStmt
    | enumStmtNoExpr
    | enumStmtInf
    | enumStmt
    | failure("Only classes, defs, includes, scopes, and imports are allowed in outer scope.")
  )

  def nestNamespaces(lst : List[String], stmts : List[AstTraversable]) : Namespace = {
    if (lst.size == 1)
      Namespace(lst.head, stmts)
    else
      Namespace(lst.head, List(nestNamespaces(lst.tail, stmts)))
  }

  def importStmt = positioned(
    "import" ~ rep1sep(importClause, ",") ^^ { case _ ~ lst => ImportAst(lst.flatten) }
  )

  def importSimple = positioned(
    rep1sep(ident, "::") ^^ {
      case lst =>
        if (lst.last == "_")
          ImportAllAst(lst)
        else
          ImportSpecificAst(lst)
    }
  )

  def importList = rep1sep(ident, "::") ~ "::" ~ "{" ~
    rep1sep((positioned(ident ^^ {case id => PosString(id)})), ",") ~ "}" ^^ {
      case flst ~ _ ~ _ ~ slst ~ _ => slst.map{x =>
        val i = ImportSpecificAst(flst :+ x.str)
        i.pos = x.pos
        i
      }
    }

  def importClause : Parser[List[AstTraversable]] = importList | importSimple ^^ { case x => List(x) }

  def namespaceStmt = positioned(
    "scope" ~ rep1sep(ident, "::") ~ "{" ~ outerStmt.* ~ "}"
    ^^ { case _ ~ lst ~ _ ~ stmts ~ _ =>
      nestNamespaces(lst, stmts)
    }
  )

  def traitList = "trait" ~> repsep(generic, ",")

  def traitStmt = positioned(
    "abstract".? ~ "chare".? ~ "trait" ~ genericDecl ~ ":".? ~ traitList.? ~ "{" ~ innerStmtList ~ "}"
    ^^ { case abs ~ ch ~ _ ~ cls ~ _ ~ tList ~ _ ~ stmts ~ _ =>
      ClassAst(false,
        true,
        if (abs.isEmpty) false else true,
        false,
        if (ch.isEmpty) false else true,
        None,
        cls,
        None,
        if (tList.isEmpty) List() else tList.get,
        stmts.to[ListBuffer]
      )
    }
  )

  def chareArrayDim = positioned(
    "array1d" ^^^ ArrayDim(1) |
    "array2d" ^^^ ArrayDim(2) |
    "array3d" ^^^ ArrayDim(3)
  )

  def enumInsideNoExpr = positioned(
    ident ^^ { case id => EnumInsideAst(Type(Identifier(List(id)), List(), None), Some(LitType(EnumCounter.next.toString(), Type(Identifier(List("int")), List(), None)))) }
  )

  def enumInside = positioned(
    ident ~ ("=" ~> expression).? ^^ {
      case id ~ ex => EnumInsideAst(Type(Identifier(List(id)), List(), None), ex)
    }
  )

  def enumStmtNoExpr = {EnumCounter.counter = 0; positioned(
    "type" ~ ident ~ "{" ~ repsep(enumInsideNoExpr, ",") ~ "}" ^^ {
      case _ ~ id ~ _ ~ enums ~ _ => EnumAst(Type(Identifier(List(id)), List(), None), Type(Identifier(List("int")), List(), None), enums)
    }
  )}

  def enumStmtInf = positioned(
    "type" ~ ident ~ ":" ~ generic ~ "{" ~ ident ~ numericLit.? ~ "." ~ "." ~ numericLit.? ~ "}" ^^ {
      case _ ~ id ~ _ ~ gen ~ _ ~ id1 ~ lo ~ _ ~ _ ~ hi ~ _ => EnumAst(Type(Identifier(List(id)), List(), None), gen,
        List(EnumInfAst(Type(Identifier(List(id1)), List(), None),lo,hi)))
    }
  )

  def enumStmt = positioned(
    "type" ~ ident ~ ":" ~ generic ~ "{" ~ repsep(enumInside, ",") ~ "}" ^^ {
      case _ ~ id ~ _ ~ gen ~ _ ~ enums ~ _ => EnumAst(Type(Identifier(List(id)), List(), None), gen, enums)
    }
  )

  def classStmt = positioned(
    "abstract".? ~ "mainchare".? ~ "chare".? ~ chareArrayDim.? ~ "template".? ~ "class" ~ genericDecl ~ ":".? ~ generic.? ~ ",".? ~ traitList.? ~ "{" ~ innerStmtList ~ "}"
    ^^ { case abs ~ mch ~ ch ~ arrDim ~ temp ~ _ ~ cls ~ _ ~ parent ~ _ ~  tList ~ _ ~ stmts ~ _ =>
      ClassAst(
        if (temp.isEmpty) false else true,
        false,
        if (abs.isEmpty) false else true,
        if (mch.isEmpty) false else true,
        if (ch.isEmpty && mch.isEmpty) false else true,
        arrDim,
        cls,
        parent,
        if (tList.isEmpty) List() else tList.get,
        stmts.to[ListBuffer]
      )
    }
  )

  def whenStmt : Parser[AstTraversable] = positioned(
    "when" ~ sdagEntryLst ~ (scope | empty) ^^ {
      case _ ~ entries ~ body => WhenAst(entries, body)
    }
  )

  def sdagEntryLst : Parser[List[SdagEntryAst]] = rep1sep(sdagEntry, ",")

  def sdagEntry : Parser[SdagEntryAst] = (
    identifier ~ "(" ~ repsep(typedParam | expression, ",").? ~ ")" ^^ {
      case id ~ _ ~ pttnLst ~ _ => SdagEntryAst(
        id,
        if (pttnLst.isEmpty) List() else pttnLst.get
      )
    }
  )

  def sdagRefNum : Parser[List[AstTraversable]] = "[" ~> rep1sep(expression, ",") <~ "]"

  def identifierLst : Parser[List[String]] = rep1sep(ident, "::")

  def identifier : Parser[Identifier] = positioned(
    identifierLst ^^ { case lst => Identifier(lst) }
  )

  def annotationLst : Parser[List[Annotation]] = rep1(annotation)

  def annotation : Parser[Annotation] = positioned(
      "@" ~> ident ^^ { case id => Annotation(id) }
    | "@" ~ ident ~ "(" ~ repsep(expression, ",") ~ ")" ^^ { case _ ~ id ~ _ ~ exprList ~ _ => Annotation(id, exprList) }
  )

  def simpleIdentifier : Parser[Identifier] = positioned(
    ident ^^ { case id => Identifier(List(id)) }
  )

  def simpleIdentifierFun : Parser[Identifier] = positioned(
      (ident | defineOps) ^^ { case id => Identifier(List(id)) }
    | "[]" ^^ { case _ => Identifier(List("[]")) }
    | "<>" ^^ { case _ => Identifier(List("<>")) }
  )

  def genericDeclFun : Parser[Type] = positioned(
    simpleIdentifierFun ~ genericDeclLst.?
    ^^ { case id ~ generic =>
      Type(id, if (generic.isEmpty) List() else generic.get, None)
    }
  )

  def genericDecl : Parser[Type] = positioned(
    simpleIdentifier ~ genericDeclLst.?
    ^^ { case id ~ generic =>
      Type(id, if (generic.isEmpty) List() else generic.get, None)
    }
  )

  def genericDeclLst : Parser[List[Type]] =
    "[" ~> repsep(identifier ~ typeConstraint.? ^^ { case id ~ typ => Type(id, List(), typ) }, ",") <~ "]"

  def typeConstraint : Parser[Type] = ":>" ~> generic

  def generic : Parser[Type] = positioned(
    identifier ~ genericLst.?
    ^^ { case id ~ generic =>
      Type(id, if (generic.isEmpty) List() else generic.get, None)
    }
  )

  def genericLst : Parser[List[Type]] = "[" ~> repsep(generic, ",") <~ "]"

  def innerStmtList : Parser[List[AstTraversable]] = (
    innerStmt.* ^^ { case inn => inn.flatten }
  )

  def innerStmt : Parser[List[AstTraversable]] = (
      consDefStmt   ^^ { case x => List(x) }
    | defStmt       ^^ { case x => List(x) }
    | importStmt    ^^ { case x => List(x) }
    | declStmtList
    | declStmt      ^^ { case x => List(x) }
    | destructStmt  ^^ { case x => List(x) }
    | annotationLst ~ innerStmt ^^ { case alst ~ x => { x.head.alst = alst; x } }
    | failure("Only defs, vals, and vars are allowed inside a class definition.")
  )

  def maybeExpr = "=" ~> expression
  def declBody = typedParam ~ maybeExpr.?
  def declStmtList : Parser[List[AstTraversable]] = {
    declStart ~ rep1sep(declBody, ",") ~ ";" ^^ {
      case dtype ~ lst ~ _ => lst.map(l =>DeclAst(dtype, l._1, l._2))
    }
  }

  def declStart = (
      "var" ^^ { case _ => true }
    | "val" ^^ { case _ => false }
  )

  def declEnd : Parser[Option[AstTraversable]] = (
      ";" ^^ { case _ => None }
    | "=" ~> expression <~ ";" ^^ { case x => Some(x) }
  )

  def declStmt : Parser[DeclAst] =  positioned(
    declStart ~ typedParam ~ declEnd ^^ {
      case dtype ~ tparam ~ ex => DeclAst(dtype, tparam, ex)
    }
  )

  def includeStmt = positioned(
    "include" ~ stringLit ^^ { case _ ~ str => IncludeAst(str) }
  )

  def maybeSet = "=".? ~ typedParam ^^ {case set ~ param => param.set = !set.isEmpty; param}

  def destructStmt = positioned(
    "def" ~ "~" ~ "this" ~ "(" ~ ")" ~ scopeCons ^^ {
      case _ ~ _ ~ _ ~ _ ~ _ ~ semi =>
        DefAst(
          List("destructor"), None,
          Type(Identifier(List("~this")), List(), None),
          List(),
          Type(Identifier(List("unit")), List(), None),
          semi)
    }
  )

  def consDefStmt : Parser[DefAst] = positioned(
    "entry".? ~ "def" ~ "this"  ~ "(" ~ repsep(maybeSet, ",").? ~ ")" ~ scopeCons
    ^^ { case entry ~ _ ~ _ ~ _ ~ typedParamList ~ _ ~ semi =>
      DefAst(
        if (entry.isEmpty) List("constructor") else List("entry","constructor"), None,
        Type(Identifier(List("this")), List(), None),
        if (typedParamList.isEmpty) List() else typedParamList.get,
        Type(Identifier(List("unit")), List(), None),
        semi)
    }
  )

  def returnOrUnit = (
      ":" ~> generic
    | success(Type(Identifier(List("unit")), List(), None))
  )

  def reducer : Parser[ReducerAst] = positioned(
    "reducer" ~> genericLst ^^  { case gdl => ReducerAst(gdl) }
  )

  def defKey : Parser[String] = "override" | "entry" | "system" | "sync" | "async"

  def defStmt : Parser[DefAst] = positioned(
    rep(defKey) ~ reducer.? ~ "def" ~ genericDeclFun  ~ "(" ~ repsep(typedParam, ",").? ~ ")" ~ returnOrUnit ~ (scope | empty)
    ^^ { case keywords ~ red ~ _ ~ generic ~ _ ~ typedParamList ~ _ ~ typeStmt ~ semi =>
      DefAst(keywords.toSet.toList, red, generic,
        if (typedParamList.isEmpty) List() else typedParamList.get,
        typeStmt,
        semi)
    }
  )

  def typedParam : Parser[TypeParamAst] = positioned(
    ident ~ ":" ~ generic
    ^^ { case ident ~ _ ~ generic => TypeParamAst(ident,generic) }
  )

  def empty : Parser[Option[List[AstTraversable]]] = ";" ^^^ None
  def scope : Parser[Option[List[AstTraversable]]] = "{" ~> semiStmtList <~ "}" ^^ { case lst => Some(lst) }
  def scopeCons : Parser[Option[List[AstTraversable]]] = "{" ~> semiStmtCons.* <~ "}" ^^ { case lst => Some(lst) }

  def semiStmtCons = semiStmtSingle | superCons

  def superCons : Parser[AstTraversable] = positioned(
    "super" ~ "(" ~ repsep(expression, ",") ~ ")" ~ ";" ^^ {
      case _ ~ _ ~ exprList ~ _ ~ _ => createFunCallID("super", exprList)
    }
  )

  def returnStmt : Parser[AstTraversable] = positioned(
    "return" ~> expression.? <~ ";" ^^ { case ex => ReturnAst(ex) }
  )

  def assignStmt : Parser[AstTraversable] = positioned(
    expression ~ "=" ~ expression ^^ { case ex1 ~ _ ~ ex2 => AssignAst(ex1,ex2) }
  )

  object NamespaceCounter {
    var counter = 1;
    def next : Int = {counter += 1; counter}
  }

  object EnumCounter {
    var counter = 0;
    def next : Int = {counter += 1; counter}
  }

  def scopeStmt : Parser[AstTraversable] = positioned(
    "{" ~ semiStmtList ~ "}" ^^ { case  _ ~ stmts ~ _ => Namespace("_" + NamespaceCounter.next, stmts) }
  )

  def elseStmt : Parser[AstTraversable] = "else" ~> semiStmtSingle

  def ifStmt : Parser[AstTraversable] = positioned(
    "if" ~ "(" ~ expression ~ ")" ~ semiStmtSingle ~ elseStmt.? ^^ {
      case  _ ~ _ ~ expr ~ _ ~ stmt ~ el => IfAst(expr, stmt, el)
    }
  )

  def whileStmt : Parser[AstTraversable] = positioned(
    "while" ~ "(" ~ expression ~ ")" ~ semiStmtSingle ^^ {
      case  _ ~ _ ~ expr ~ _ ~ stmt => WhileAst(expr, stmt)
    }
  )

  def doWhileStmt : Parser[AstTraversable] = positioned(
    "do" ~ semiStmtSingle ~ "while" ~ "(" ~ expression ~ ")" ~ ";" ^^ {
      case  _ ~ stmt ~ _ ~ _ ~ expr ~ _ ~ _ => DoWhileAst(expr, stmt)
    }
  )

  def declForStmt : Parser[DeclAst] = positioned(
    declStart ~ typedParam ~ ("=" ~> expression).? ^^ {
      case dtype ~ tparam ~ ex => DeclAst(false, tparam, ex)
    }
  )

  def forStmt : Parser[AstTraversable] = positioned(
    "for" ~ "(" ~ repsep(declForStmt | assignStmt | expression, ",") ~ ";" ~ repsep(expression, ",") ~ ";" ~ repsep(expression, ",") ~ ")" ~ semiStmtSingle ^^ {
      case _ ~ _ ~ decls ~ _ ~ expr1 ~ _ ~ expr2 ~ _ ~ stmt => {
        // wrap in a namespace so the decls are not in the enclosing scope
        Namespace("_" + NamespaceCounter.next, List(ForAst(decls, expr1, expr2, stmt)))
      }
    }
  )

  def semiStmtList : Parser[List[AstTraversable]] = semiStmt.* ^^ { case x => x.flatten }

  def semiStmt : Parser[List[AstTraversable]] = (
      declStmt          ^^ { case x => List(x) }
    | declStmtList
    | semiStmtSingle    ^^ { case x => List(x) }
  )

  def semiStmtSingle : Parser[AstTraversable] = positioned(
      returnStmt
    | assignStmt <~ ";"
    | scopeStmt
    | ifStmt
    | expression <~ ";"
    | importStmt
    | whileStmt
    | doWhileStmt
    | forStmt
    | declStmt
    | whenStmt
    | annotationLst ~ semiStmtSingle ^^ { case alst ~ x => { x.alst = alst; x } }
  )

  def expression : Parser[AstTraversable] = positioned(
     opPrec(opOrder, dotExpr)
  )

  def prefixOps = "^" | "+" | "-" | "$"  | "!"
  def unaryOpsParan = "#" | "?" | "++" | "--" // | "@"
  def binaryOps = "+" | "-" | "/" | "*" | "%" | "&&" | "||" |
                  "+=" | "-=" | "<" | ">" | "<=" | ">=" | "!=" | "=="
  def defineOps = unaryOpsParan | binaryOps | prefixOps

  def opOrder =
    List(
      ("+=" | "-="),
      ("&&" ^^ {case x => x} ),
      ("||" ^^ {case x => x} ),
      ("<" | ">" | "<=" | ">=" | "!=" | "=="),
      ("+" | "-"),
      ("/" | "*" | "%")
    )

  def opPrec(x : List[Parser[String]], next : Parser[AstTraversable]) : Parser[AstTraversable] = (
    if (x.size == 0) next
    else
      (opPrec(x.tail,next) ~ rep((x.head ~ opPrec(x.tail,next)) ^^ {
        case op ~ expr => createFunCallID(op, List(expr)) }
      ) ^^ { case el ~ rest => el::rest }
      ) ^^ { case lst => if (lst.size > 1) BinOp(lst, ".") else lst.head }
  )

  def dotExpr : Parser[AstTraversable] = (
    (call ~ rep(("." ~> call | noArgSym | brackSym | otherSym)) ^^ { case el ~ rest => el::rest })
      ^^ { case lst => BinOp(lst, ".") }
  )

  def noArgSym = unaryOpsParan ^^ {case id => createFunCallID(id, List()) }
  def brackSym = positioned("[" ~ repsep(expression, ",") ~ "]" ^^ {
    case _ ~ exprList ~ _ => createFunCallID("[]", exprList)
  })
  def otherSym = positioned("<" ~ repsep(expression, ",") ~ ">" ^^ {
    case _ ~ exprList ~ _ => createFunCallID("<>", exprList)
  })
  def binaryEx = positioned(binaryOps ~ call ^^ {
    case op ~ expr => createFunCallID("op", List(expr))
  })

  def fcall : Parser[FuncCall] = positioned(
    (   identifier
      | defineOps ^^ {case id => Identifier(List(id))}
    ) ~ genericLst.? ~ "(" ~ repsep(expression, ",") ~ ")" ^^ {
        case id ~ gen ~ _ ~ exprList ~ _ => FuncCall(Type(id, gen.getOrElse(List()), None), exprList)
      }
  )

  def nproxy : Parser[List[AstTraversable]] = "(" ~> repsep(expression, ",") <~ ")"

  def call : Parser[AstTraversable] = positioned(
      fcall
    | noArgSym
    | identifier
    | "(" ~> expression <~ ")"
    | "new" ~> fcall ^^ {
      case fc =>  fc.skipGen = true; NewAst(fc, List())
    }
    | ("proxy" ~> nproxy.? <~ "new") ~ fcall ^^ {
      case nprox ~ fc =>  {
        fc.skipGen = true
        val x = NewAst(fc, if (nprox.isEmpty) List() else nprox.get)
        x.isProxyNew = true
        x
      }
    }
    | "delete" ~> expression ^^ { case ex =>  DeleteAst(ex) }
    | prefixOps ~ expression ^^ { case op ~ ex =>  FuncCall(Type(Identifier(List(op)),List(),None), List(ex)) }
    | "async" ~> expression ^^ { case ex =>  AsyncAst(ex) }
    | numericLit ^^ { case lit =>
      if (lit.contains("."))
        LitType(lit, Type(Identifier(List("double")), List(), None))
      else
        LitType(lit, Type(Identifier(List("int")), List(), None))
      // LitType(lit, Type(Identifier(List("float")), List(), None))
    }
    | stringLit ^^ { case lit =>
      val m = LitType(lit, Type(Identifier(List("string")), List(), None))
      m.string = true
      m
    }
    | "true" ^^ { case _ => LitType("true", Type(Identifier(List("boolean")), List(), None)) }
    | "false" ^^ { case _ => LitType("false", Type(Identifier(List("boolean")), List(), None)) }
    | "null" ^^ { case _ => LitType("NULL", Type(Identifier(List("any")), List(), None)) }
    | "this" ^^ { case _ => LitType("this", Type(Identifier(List("any")), List(), None)) }
    | "thisProxy" ^^ { case _ => LitType("thisProxy", Type(Identifier(List("any")), List(), None)) }
    | "thisIndex" ^^ { case _ => LitType("thisIndex", Type(Identifier(List("any")), List(), None)) }
  )
}
