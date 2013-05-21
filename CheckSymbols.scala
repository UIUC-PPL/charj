package CharjParser

import scala.util.parsing.input.{Positional,Position}

class Checker(tree : Stmt) {
  import Checker._
  import BaseContext.verbose

  def start() = {
    if (verbose) println("--- traverse print classes ---")
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt]
    def printClass(cls : Stmt) = if (verbose) println("found class name = " + cls.asInstanceOf[ClassStmt].name)
    new StmtVisitor(tree, filterClass, printClass);

    if (verbose) println("--- traverse resolve class type ---")
    new StmtVisitor(tree, filterClass, determineClassType);

    if (verbose) println("--- traverse resolve def type ---")
    def filterDefs(cls : Stmt) = cls.isInstanceOf[DefStmt]
    new StmtVisitor(tree, filterDefs, determineDefType)

    if (verbose) println("--- traverse resolve decl type ---")
    def filterDecls(cls : Stmt) = cls.isInstanceOf[DeclStmt]
    new StmtVisitor(tree, filterDecls, determineDeclType)

    if (verbose) println("--- traverse expressions ---")
    new ExprVisitor(tree, determineExprType)

    if (verbose) println("--- traverse statements with expressions ---")
    def filterNone(cls : Stmt) = true
    new StmtVisitor(tree, filterNone, checkStmtType)
  }
}

object Checker {
  import BasicTypes._
  import BaseContext.verbose

  def checkStmtType(tree : Stmt) {
    tree match {
      case t@AssignStmt(lval, _, rval) => {
        val rsym = rval.sym
        val (retType, context) = resolveIdentType(tree, null, tree.context, lval)

        if (retType.isInstanceOf[BoundClassSymbol] && rsym != null) {
          t.sym = retType.asInstanceOf[BoundClassSymbol]
          if (!classesEqual(t.sym, rsym))
            SemanticError("tried to assign to different class type: " + t.sym, rval.pos)
        } else {
          if (rsym == null)
            SemanticError("could not find type of expr", rval.pos)
          else
            SemanticError("could not find return type for ident " + lval, t.pos)
        }
      }
      case t@IfStmt(cond, _, _) => {
        if (cond.sym == null)
          SemanticError("if statement condition type not determined", t.pos)
        if (!classesEqual(cond.sym, resolveClassType(booleanType,tree)))
          SemanticError("if statement condition must be of type boolean", t.pos)
      }
      case t@WhileStmt(expr1, _) => {
        if (expr1.sym == null)
          SemanticError("while statement condition type not determined", t.pos)
        if (!classesEqual(expr1.sym, resolveClassType(booleanType,tree)))
          SemanticError("while statement condition must be of type boolean", t.pos)
      }
      case _ => ;
    }
  }

  def determineDefType(tree : Stmt) {
    tree.asInstanceOf[DefStmt] match {
      case t@DefStmt(_, name, maybeParams, hasRet, lst) => {
        val lstParams = if (maybeParams.isEmpty) List() else maybeParams.get
        val inTypes = lstParams.map(tparam => resolveClassType(tparam.typ, tree))
        val retType = hasRet match {
          case Some(x) => resolveClassType(x, tree)
          case None => resolveClassType(unitType, tree)
        }
        if (t.sym == null) {
          SemanticError("could not resolve def types", t.pos)
          //System.exit(5)
        } else {
          t.sym.inTypes = inTypes
          t.sym.retType = retType
          if (verbose) println(name + " def ret is " + retType + ", inTypes = " + inTypes)
        }
      }
    }
  }

  def determineClassType(tree : Stmt) {
    tree.asInstanceOf[ClassStmt] match {
      case t@ClassStmt(name, _, _, Some(parent), _) => {
        val cls = resolveClassType(parent, tree)
        if (cls != null)
          t.context.extensions += cls.cs.context
        if (verbose) println(name + " this class symbol is " + t.sym)
        if (cls != null)
          t.sym.subtypes += cls
        if (verbose) println(name + " resolved to subclass of " + cls)
      }
      case _ => ;
    }
  }

  def determineDeclType(tree : Stmt) {
    tree.asInstanceOf[DeclStmt] match {
      case t@DeclStmt(_, name, Some(typ), _) => {
        val sym = resolveClassType(typ, tree)
        val thisSym = tree.context.resolve(_ match {
          case DeclSymbol(n, _) => n == name
          case _ => false
        })
        if (sym == NoSymbol())
          SemanticError(name + " not resolved", t.pos)
        else
          if (verbose) println("resolved " + name + " to " + thisSym.get + " with type: " + sym)
        thisSym.get.asInstanceOf[DeclSymbol].declType = sym.asInstanceOf[BoundClassSymbol]
      }
      case _ => ;
    }
  }

  def determineExprType(expr : Expression, cls : Stmt) {
    expr match {
      case FunExpr(name, param) => {
        if (verbose) println("determing type for FunExpr: " + name)
        val exprs = if (param.isEmpty) List() else param.get
        val types = exprs.map(_.sym)
        val (identSym, context) = resolveIdentType(cls, null, cls.context, name.dropRight(1))
        val retType = resolveFunType(cls, name.last, context, types)

        if (retType.isInstanceOf[BoundClassSymbol])
          expr.sym = retType.asInstanceOf[BoundClassSymbol]
        else
          SemanticError("could not find return type for def " + name, expr.pos)
      }
      case NumLiteral(str) => {
        val theInt = tryConvertToInt(str)
        if (!theInt.isEmpty)
          expr.sym = resolveClassType(intType, cls)
      }
      case StrExpr(lst) => {
        val (retType, context) = resolveIdentType(cls, null, cls.context, lst)
        if (retType.isInstanceOf[BoundClassSymbol])
          expr.sym = retType.asInstanceOf[BoundClassSymbol]
        else
          SemanticError("could not find return type for ident " + lst, expr.pos)
      }
      case True() | False() => expr.sym = resolveClassType(booleanType, cls)
      case AddExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case DivExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case SubExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case MulExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case AndExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case OrrExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case ComExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(booleanType, cls))
      case LesExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(booleanType, cls))
      case LeqExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(booleanType, cls))
      case GesExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(booleanType, cls))
      case GeqExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(booleanType, cls))
      case NotExpr(l) => {
        if (!classesEqual(l.sym, resolveClassType(booleanType, cls)))
          SemanticError("boolean negation incorrect type: " + l.sym, l.pos)
        else expr.sym = l.sym
      }
      case NegExpr(l) => {
        if (!classesEqual(l.sym, resolveClassType(intType, cls)))
          SemanticError("int negation incorrect type: " + l.sym, l.pos)
        else expr.sym = l.sym
      }
      case _ => ;
    }
  }

  def tryConvertToInt(s : String) : Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

  def checkBinarySet(l : Expression, r : Expression, cur : Expression, ret : BoundClassSymbol) {
    if (!classesEqual(l.sym, r.sym))
      SemanticError("binary op " + cur + " with different types: " + l.sym + " at " + l.pos +  ", " + r.sym, r.pos)
    cur.sym = ret
  }

  def resolveIdentType(cls : Stmt, curSymbol : Symbol, context : Context, n : List[String]) : (Symbol, Context) = {
    if (verbose) println("recursive resolveIdentType: " + n)

    if (n.size != 0) {
      val ident = n.head
      var newContext : Context = null

      val decl = expectDeclSymbol(context, cls, ident)
      if (!decl.isEmpty) {
        val typ = decl.get.declType
        if (verbose) println("\tresolved type for " + n.head + " = " + typ)
        if (typ != null && typ.cs != null && typ.cs.context != null) {
          val newContext = typ.cs.context
          return resolveIdentType(cls, typ, newContext, n.tail)
        } else {
          return (NoSymbol(), BaseContext.context)
        }
      } else {
        SemanticError("could not resolve type for: " + ident, cls.pos)
        return (NoSymbol(), BaseContext.context)
      }
    } else {
      return (curSymbol, context)
    }
  }

  def resolveFunType(cls : Stmt, methodName : String, context : Context, lst : List[Symbol]) : Symbol = {
    val foundSym = context.resolve(_ match {
      case t@DefSymbol(name) => {
        if (name == methodName && t.inTypes.size == lst.size) {
          val toComp = (t.inTypes,lst).zipped.toList
          var isMatching = true
          for ((type1, type2) <- toComp) {
            if (type1 != type2) isMatching = false
          }
          isMatching
        } else false
      }
      case _ => false
    })

    if (foundSym.isEmpty) {
      SemanticError("def " + methodName + ", in = " + lst + ", unknown, searched context: " + context, cls.pos)
      NoSymbol()
      //System.exit(1)
    } else {
      if (verbose) println("resolved def to: " + foundSym.get)
      foundSym.get.asInstanceOf[DefSymbol].retType
    }
  }

  def expectDeclSymbol(context : Context, cls : Stmt, str2 : String) : Option[DeclSymbol] = {
    val sym = context.resolve(_ match {
      // make sure it was defined before if it's the same context
      case t@DeclSymbol(str, _) => str == str2 && (t.pos < cls.pos || cls.context != context || !context.ordered)
      case _ => false 
    })

    if (sym.isEmpty) {
      SemanticError("symbol " + str2 + " unknown", cls.pos)
      //System.exit(1)
      None
    } else {
      sym.get match {
        case t@DeclSymbol(_, _) => Some(t)
        case _ => None
      }
    }
  }

  def resolveClassType(t : Type, tree : Stmt) : BoundClassSymbol = {
    // @todo for now namespaces for types is not supported
    if (t.name.size != 1) {
      SemanticError("type " + t + " not supported", t.pos)
      return null
    }

    val genTypes : List[Symbol] = t.generic match {
      case Some(typeList) => typeList.map(t => resolveClassType(t, tree))
      case None => List()
    }

    val className = t.name.head
    val optSym = tree.context.resolve(
      _ match {
        case ClassSymbol(name, num) => if (name != className || num != genTypes.size) false else true
        case _ => false
      }
    )

    if (optSym.isEmpty) {
      SemanticError("could not resolve type " + t.name.head, t.pos)
      //System.exit(1)
      null
    } else {
      optSym.get match {
        case t@ClassSymbol(_, _) => BoundClassSymbol(t, genTypes)
      }
    }
  }

  def classesEqual(cls1 : BoundClassSymbol, cls2 : BoundClassSymbol) : Boolean = {
    cls1 == cls2
  }
}
