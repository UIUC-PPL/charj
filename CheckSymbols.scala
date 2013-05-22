package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.ArrayBuffer

class Checker(tree : Stmt) {
  import Checker._
  import BaseContext.verbose

  def start() = {
    if (verbose) println("--- traverse print classes ---")
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt]
    def printClass(cls : Stmt) = if (verbose) println("found class name = " + cls.asInstanceOf[ClassStmt].name +
                                                      ", abstract = " + cls.asInstanceOf[ClassStmt].isAbstract)
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
      case t@DeclStmt(_, _, optType, Some(expr1)) => {
        if (expr1.sym == null)
          SemanticError("decl statement condition type not determined", t.pos)
        if (optType.isEmpty)
          SemanticError("type required on decl", t.pos)
        if (!classesEqual(expr1.sym, resolveClassType(optType.get, tree))) {
          val typ = resolveClassType(optType.get, tree)
          SemanticError("decl type and expression must match: " + expr1.sym + " and " + typ, t.pos)
        }
      }
      case t@ReturnStmt(optExp) => {
        if (optExp.isEmpty) {
          if (!classesEqual(t.enclosingDef.sym.retType, resolveClassType(unitType,tree)))
            SemanticError("return type of def must be unit (or not present)", t.pos)
        } else if (optExp.get.sym != null) {
          if (!classesEqual(t.enclosingDef.sym.retType, optExp.get.sym))
            SemanticError("return type of def must match declared: " + optExp.get.sym, t.pos)
        } else {
          SemanticError("unable to resolve type of return", t.pos)
        }
      }
      case _ => ;
    }
  }

  def determineDefType(tree : Stmt) {
    tree.asInstanceOf[DefStmt] match {
      case t@DefStmt(_, name, maybeParams, hasRet, lst) => {
        val lstParams = if (maybeParams.isEmpty) List() else maybeParams.get
        val inTypes = lstParams.map {tparam =>
          val decl = expectDeclSymbol(lst.context, tparam, tparam.name)
          val sym = resolveClassType(tparam.typ, tree)
          if (decl.isEmpty)
            SemanticError("could not find symbol for parameter: " + tparam, tparam.pos)
          else
            decl.get.declType = sym
          tparam.sym = sym
          sym
        };
        var retType = hasRet match {
          case Some(x) => resolveClassType(x, tree)
          case None => resolveClassType(unitType, tree)
        }
        if (t.isConstructor) {
          retType = resolveClassType(Type(List(t.enclosingClass.name), t.enclosingClass.generic), tree);
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
        if (cls != null) {
          if (verbose) println("adding context extension to " + name + ": for " + cls)
          t.context.extensions += cls.cs.context
        }
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
        val thisSym = tree.context.resolve{a : (Symbol,Context) => a._1 match {
          case DeclSymbol(n, _) => n == name
          case _ => false
        }}
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

        if (verbose) println("function call: " + name + ", ident = " + identSym)

        var retType = resolveFunType(cls, name.last, context, types)
        if (retType.isInstanceOf[BoundClassSymbol])
          retType = replaceWithBinding(retType.asInstanceOf[BoundClassSymbol], identSym)

        if (retType.isInstanceOf[BoundClassSymbol]) {
          expr.sym = retType.asInstanceOf[BoundClassSymbol]
          expr.context = retType.asInstanceOf[BoundClassSymbol].cs.context
        } else
          SemanticError("could not find return type for def " + name, expr.pos)
      }
      case NumLiteral(str) => {
        val theInt = tryConvertToInt(str)
        if (!theInt.isEmpty)
          expr.sym = resolveClassType(intType, cls)
      }
      case StrExpr(lst) => {
        val (retType, context) = resolveIdentType(cls, null, cls.context, lst)

        if (retType.isInstanceOf[BoundClassSymbol]) {
          expr.sym = retType.asInstanceOf[BoundClassSymbol]
          expr.context = context
        } else
          SemanticError("could not find type for ident " + lst, expr.pos)
      }
      case True() | False() => expr.sym = resolveClassType(booleanType, cls)
      case DotExpr(l, r) => {
        if (l.sym == null) {
          SemanticError("should be resolved to type" + l, l.pos)
        } else {
          if (verbose) println("resolved to type: " + l.sym)

          val (rt, context) = resolveIdentType(cls, l.sym, l.context, List(r.text))
          var retType = rt
          if (retType.isInstanceOf[BoundClassSymbol])
            retType = replaceWithBinding(retType.asInstanceOf[BoundClassSymbol], l.sym)

          if (retType.isInstanceOf[BoundClassSymbol]) {
            r.sym = retType.asInstanceOf[BoundClassSymbol]
            r.context = context
            expr.sym = r.sym
            expr.context = context
          } else
            SemanticError("could not find type for DotExpr rhs " + r, expr.pos)
        }
      }
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
      case Null() => expr.sym = resolveClassType(refType, cls)
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
    if (n.size != 0 && verbose)
      println("recursive resolveIdentType: " + n + ", context = " + context)

    if (n.size != 0) {
      val ident = n.head
      var newContext : Context = null
      val decl = expectDeclSymbol(context, cls, ident)

      if (!decl.isEmpty) {
        var typ = decl.get.declType
        //for ((g1, g2) <- typ.generics zip curSymbol.generics)
        if (typ.isInstanceOf[BoundClassSymbol] && curSymbol.isInstanceOf[BoundClassSymbol]) {
          //println("typ = " + typ)
          //println("curSymbol = " + curSymbol)
          typ = replaceWithBinding(typ, curSymbol)
          if (verbose) println("**** newTyp = " + typ)
        }
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
    if (verbose) println("trying to resolve function " + methodName)
    val foundSym = context.resolve{a : (Symbol,Context) => a._1 match {
      case t@DefSymbol(name, _) => {
        if (name == methodName && t.inTypes.size == lst.size) {
          val toComp = (t.inTypes,lst).zipped.toList
          var isMatching = true
          for ((type1, type2) <- toComp) {
            if (type1.isInstanceOf[BoundClassSymbol] && type2.isInstanceOf[BoundClassSymbol]) {
              val bcs1 = type1.asInstanceOf[BoundClassSymbol]
              val bcs2 = type2.asInstanceOf[BoundClassSymbol]
              if (!classesEqual(bcs1, bcs2)) isMatching = false
            } else {
              if (type1 != type2) isMatching = false
            }
          }
          isMatching
        } else false
      }
      case _ => false
    }}

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
    if (context == null) {
      SemanticError("trying to search null context for: " + str2, cls.pos)
      return None
    }

    if (cls.enclosingClass != null && cls.enclosingClass.sym != null) {
      cls.enclosingClass.sym
    }

    val sym = context.resolve{a : (Symbol,Context) => a._1 match {
      // make sure it was defined before if it's the same context
      case t@DeclSymbol(str, _) => str == str2 && (t.pos < cls.pos || cls.context != a._2 || !a._2.ordered)
      case _ => false 
    }}

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

    val genTypes : ArrayBuffer[Symbol] = t.generic match {
      case Some(typeList) => ArrayBuffer(typeList.map(t => resolveClassType(t, tree)) : _*)
      case None => ArrayBuffer()
    }

    val className = t.name.head
    val optSym = tree.context.resolve{a : (Symbol,Context) => a._1 match {
        case ClassSymbol(name, num) => if (name != className || num != genTypes.size) false else true
        case _ => false
      }
    }

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
    if (cls1 == null || cls2 == null || cls1.cs == null || cls2.cs == null)
      return false;
    if (cls1.cs == cls2.cs) {
      val gens = cls1.generics zip cls2.generics
      for ((g1, g2) <- gens) {
        if (verbose) println("compare: g1 = " + g1 + ", g2 = " + g2)
        if (!(g1 == g2 || g1.isAnything || g2.isAnything)) return false
      }
      true
    } else if (cls1.isAnything || cls2.isAnything) {
      true
    } else {
      false
    }
  }

  import scala.collection.mutable.ListBuffer
  def tryFind(str : String, mapping : ListBuffer[(String, Symbol)]) : Option[Symbol] = {
    for ((str2, sym) <- mapping) {
      if (str == str2) return Some(sym)
    }
    return None
  }

  def findInMappingOrKeep(cs : BoundClassSymbol, mapping : ListBuffer[(String, Symbol)]) : BoundClassSymbol = {
    val name = cs.cs.name
    val optNewSymbol = tryFind(name, mapping)
    var curSymbol = cs

    if (!optNewSymbol.isEmpty && optNewSymbol.get.isInstanceOf[BoundClassSymbol] && cs.isAnything) {
      curSymbol = optNewSymbol.get.asInstanceOf[BoundClassSymbol].copy()
      if (verbose) println("findInMappingOrKeep: new symbol = " + curSymbol)
    }

    for ((sym, i) <- curSymbol.generics.zipWithIndex) {
      val sym2 = sym.asInstanceOf[BoundClassSymbol]
      if (sym2.isAnything && !tryFind(sym2.cs.name, mapping).isEmpty) {
        curSymbol.generics(i) = tryFind(sym2.cs.name, mapping).get.asInstanceOf[BoundClassSymbol].copy()
      }
    }

    return curSymbol
  }

  def replaceWithBinding(toBind : BoundClassSymbol, type2 : Symbol) : BoundClassSymbol = {
    if (type2.isInstanceOf[BoundClassSymbol]) {
      val hasBind = type2.asInstanceOf[BoundClassSymbol]
      val mapping = hasBind.cs.names zip hasBind.generics
      val newSymbol = findInMappingOrKeep(toBind, mapping)
      return newSymbol
    } else toBind
  }
}
