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

    // @todo need to visit FunExpr which have types
    if (verbose) println("--- traverse resolve free vars ---")
    def filterType(cls : Stmt) = cls.isInstanceOf[Type]
    new StmtVisitor(tree, filterType, findFreeVars);

    if (verbose) println("--- traverse resolve decl type ---")
    def filterDecls(cls : Stmt) = cls.isInstanceOf[DeclStmt] || cls.isInstanceOf[TypeParam]
    new StmtVisitor(tree, filterDecls, determineDeclType)

    if (verbose) println("--- traverse resolve def type ---")
    def filterDefs(cls : Stmt) = cls.isInstanceOf[DefStmt]
    new StmtVisitor(tree, filterDefs, determineDefType)

    if (verbose) println("--- traverse expressions ---")
    new ExprVisitor(tree, determineExprType)

    // if (verbose) println("--- traverse statements with expressions ---")
    // def filterNone(cls : Stmt) = true
    // new StmtVisitor(tree, filterNone, checkStmtType)
  }
}

object Checker {
  import BasicTypes._
  import BaseContext.verbose

  // def checkStmtType(tree : Stmt) {
  //   tree match {
  //     case t@AssignStmt(lval, _, rval) => {
  //       val rsym = rval.sym
  //       val (retType, context) = resolveIdentType(tree, null, tree.context, lval)

  //       if (retType.isInstanceOf[BoundClassSymbol] && rsym != null) {
  //         t.sym = retType.asInstanceOf[BoundClassSymbol]
  //         if (!classesEqual(t.sym, rsym))
  //           SemanticError("tried to assign to different class type: " + t.sym, rval.pos)
  //       } else {
  //         if (rsym == null)
  //           SemanticError("could not find type of expr", rval.pos)
  //         else
  //           SemanticError("could not find return type for ident " + lval, t.pos)
  //       }
  //     }
  //     case t@IfStmt(cond, _, _) => {
  //       if (cond.sym == null)
  //         SemanticError("if statement condition type not determined", t.pos)
  //       if (!classesEqual(cond.sym, resolveClassType(booleanType,tree)))
  //         SemanticError("if statement condition must be of type boolean", t.pos)
  //     }
  //     case t@WhileStmt(expr1, _) => {
  //       if (expr1.sym == null)
  //         SemanticError("while statement condition type not determined", t.pos)
  //       if (!classesEqual(expr1.sym, resolveClassType(booleanType,tree)))
  //         SemanticError("while statement condition must be of type boolean", t.pos)
  //     }
  //     case t@DeclStmt(_, _, optType, Some(expr1)) => {
  //       if (expr1.sym == null)
  //         SemanticError("decl statement condition type not determined", t.pos)
  //       if (optType.isEmpty)
  //         SemanticError("type required on decl", t.pos)
  //       if (!classesEqual(expr1.sym, resolveClassType(optType.get, tree))) {
  //         val typ = resolveClassType(optType.get, tree)
  //         SemanticError("decl type and expression must match: " + expr1.sym + " and " + typ, t.pos)
  //       }
  //     }
  //     case t@ReturnStmt(optExp) => {
  //       if (optExp.isEmpty) {
  //         if (!classesEqual(t.enclosingDef.sym.retType, resolveClassType(unitType,tree)))
  //           SemanticError("return type of def must be unit (or not present)", t.pos)
  //       } else if (optExp.get.sym != null) {
  //         if (!classesEqual(t.enclosingDef.sym.retType, optExp.get.sym))
  //           SemanticError("return type of def must match declared: " + optExp.get.sym, t.pos)
  //       } else {
  //         SemanticError("unable to resolve type of return", t.pos)
  //       }
  //     }
  //     case _ => ;
  //   }
  // }

  def findFreeVars(tree : Stmt) {
    tree.asInstanceOf[Type] match {
      case t@Type(term) => t.full = checkTerm(term, tree)
    }
  }

  def checkTerm(t : Term, tree : Stmt) : Term = {
    t match {
      case Bound(t) => Bound(t)
      case MVar(t) => MVar(t)
      case Fun(n, terms) => Fun(n, terms.map{checkTerm(_, tree)})
      case Tok(_) => checkTermString(t, tree)
      case _ => null
    }
  }

  def checkTermString(t : Term, tree : Stmt) : Term = {
    val cls = resolveClassType(Type(t), tree)
    println("checking term string: " + t + ", cls = " + cls)
    // no substitution needed because these are resolved terminals by definition
    cls.cs.t
  }

  def determineDefType(tree : Stmt) {
    tree.asInstanceOf[DefStmt] match {
      case t@DefStmt(_, name, maybeParams, hasRet, lst) => {
        println("resolving type for def: " + t);
        val lstParams = if (maybeParams.isEmpty) List() else maybeParams.get
        val inTypes = lstParams.map {tparam =>
          println("\tresolving type for param: " + tparam)
          val (sym,term,con) = findDeclType(lst.context, tparam, tparam.name)
          tparam.sym = sym
          sym
        };
        println("\tresolving type for ret: " + hasRet);
        var retType = hasRet match {
          case Some(x) => resolveClassType(x, tree)
          case None => resolveClassType(Type(unitType), tree)
        }
        if (t.isConstructor) retType = resolveClassType(t.enclosingClass.getType(), tree)
        if (t.sym == null) {
          SemanticError("could not resolve def types", t.pos)
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
        if (verbose) println("determine parent type of " + t.sym)
        if (cls != null) t.context.extensions += cls
        if (verbose) println("\t subclass of " + cls)
      }
      case _ => ;
    }
  }

  def determineDeclType(tree : Stmt) : BoundClassSymbol = {
    var n1 : String = ""
    var t1 : Type = null
    tree match {
      case t@DeclStmt(_, name, Some(typ), _) => { n1 = name; t1 = typ }
      case t@TypeParam(name, typ) => { n1 = name ; t1 = typ }
      case _ => {
        SemanticError("cannot determine decl type", tree.pos)
        return null
      }
    }
    println("determineDeclType: " + n1)
    val sym = resolveClassType(t1, tree)
    val thisSym = tree.context.resolve{a : (Symbol,Context) => a._1 match {
      case DeclSymbol(n, _) => n == n1
      case _ => false
    }}
    if (thisSym == NoSymbol())
      SemanticError(n1 + " not resolved", tree.pos)
    else if (verbose) println("resolved " + n1 + " to " + thisSym.get + " with type: " + sym)

    thisSym.get.asInstanceOf[DeclSymbol].declType = sym.asInstanceOf[BoundClassSymbol]
    sym.asInstanceOf[BoundClassSymbol]
  }

  def determineExprType(expr : Expression, cls : Stmt) {
    expr match {
      case FunExpr(name, gens, param) => {
        if (verbose) println("determing type for FunExpr: " + name)
        val exprs = if (param.isEmpty) List() else param.get
        val types : List[BoundClassSymbol] = exprs.map(_.sym)
        val (sym, term, con) = findIdentType(cls, null, cls.context, name.dropRight(1), null)

        if (verbose) println("function call: " + name + ", sym = " + sym)

        var (sym2, term2, con2) = findFunType(cls, name.last, con, types, gens)

        expr.sym = sym2
        expr.context = con2
      }
      case NumLiteral(str) => {
        val theInt = tryConvertToInt(str)
        if (!theInt.isEmpty)
          expr.sym = resolveClassType(Type(intType), cls)
      }
      case StrExpr(lst) => {
        val (sym, term, con) = findIdentType(cls, null, cls.context, lst, null)
        println("check type of StrExpr: " + sym)
        expr.sym = sym
        expr.context = con
      }
      case True() | False() => expr.sym = resolveClassType(Type(booleanType), cls)
      // case DotExpr(l, r) => {
      //   if (l.sym == null) {
      //     SemanticError("should be resolved to type" + l, l.pos)
      //   } else {
      //     if (verbose) println("resolved to type: " + l.sym)

      //     val (sym, term, con) = findIdentType(cls, l.sym.cs.t, l.context, List(r.text))
      //     var retType = r.t
      //     // @TODO do something here
      //     //if (retType.isInstanceOf[BoundClassSymbol])
      //       //retType = replaceWithBinding(retType.asInstanceOf[BoundClassSymbol], l.sym)

      //     if (retType.isInstanceOf[BoundClassSymbol]) {
      //       r.sym = retType.asInstanceOf[BoundClassSymbol]
      //       r.context = context
      //       expr.sym = r.sym
      //       expr.context = context
      //     } else
      //       SemanticError("could not find type for DotExpr rhs " + r, expr.pos)
      //   }
      // }
      // case AddExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      // case DivExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      // case SubExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      // case MulExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      // case AndExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      // case OrrExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      // case ComExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType), cls))
      // case LesExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType), cls))
      // case LeqExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType), cls))
      // case GesExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType), cls))
      // case GeqExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType), cls))
      // case NotExpr(l) => {
      //   if (!classesEqual(l.sym, resolveClassType(Type(booleanType), cls)))
      //     SemanticError("boolean negation incorrect type: " + l.sym, l.pos)
      //   else expr.sym = l.sym
      // }
      // case NegExpr(l) => {
      //   if (!classesEqual(l.sym, resolveClassType(Type(intType), cls)))
      //     SemanticError("int negation incorrect type: " + l.sym, l.pos)
      //   else expr.sym = l.sym
      // }
      // case Null() => expr.sym = resolveClassType(Type(refType), cls)
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

  def findIdentType(cls : Stmt, t : Term, context : Context, n : List[String], sym : BoundClassSymbol) : (BoundClassSymbol, Term, Context) = {
    if (n.size != 0 && verbose) println("recursive findIdentType: " + n + ", context = " + context)

    if (n.size != 0) {
      val (sym,term,con) = findDeclType(context, cls, n.head)
      if (verbose) println("\tresolved type for " + n.head + " = " + term)
      findIdentType(cls, term, con, n.tail, sym)
    } else (sym, t, context)
  }

  def findFunType(cls : Stmt, methodName : String, context : Context, lst : List[BoundClassSymbol], gens : List[Term]) : (BoundClassSymbol, Term, Context) = {
    if (verbose) println("trying to resolve function " + methodName)
    val sym = context.resolve{a : (Symbol,Context) => a._1 match {
      case t@DefSymbol(name, _) => {
        if (name == methodName && t.inTypes.size == lst.size) {
          val toComp = (t.inTypes,lst).zipped.toList
          var isMatching = true
          for ((t1, t2) <- toComp) {
            val u = Unifier(true)
            val sub1 = u.subst(t1.cs.t, t1.bindings)
            val sub2 = u.subst(t2.cs.t, t2.bindings)
            println("t1 = " + sub1 + ", t2 = " + sub2)
            if (u.hasError) isMatching = false
          }
          isMatching
        } else false
      }
      case _ => false
    }}

    if (sym.isEmpty)
      SemanticError("def " + methodName + ", in = " + lst + ", unknown, searched context: " + context, cls.pos)

    val d = sym.get.asInstanceOf[DefSymbol]
    var nb : List[(Term,Term)] = d.retType.bindings

    if (gens.length > 0) {
      val t1 : Term = Fun(methodName, gens)
      val t2 : Term = d.retType.cs.t
      println("\t has gens: checking between: " + t1 + " vs " + t2)
      nb = Unifier(true).unifyTerm(t1, t2, d.retType.bindings);
    }

    val (nt, ncon) = findNew(d.retType, nb)

    if (d.isCons) {
      // unify with class decl to ensure return type of constructor has equal arity
      Unifier(true).unifyTerm(d.classCons.sym.t, nt, List())
    }

    if (verbose) println("resolved def rettype to: " + d.retType + ", new term = " + nt)
    (d.retType, nt, ncon)
  }

  def findNew(sym : BoundClassSymbol, bindings : List[(Term,Term)] = List()) : (Term, Context) = {
    val nt = Unifier(true).subst(sym.cs.t, sym.bindings ++ bindings)
    if (nt == sym.cs.t) (nt, sym.cs.context)
    else (nt, resolveClassType(Type(nt)).cs.context)
  }

  def findDeclType(context : Context, cls : Stmt, ident : String) : (BoundClassSymbol, Term, Context) = {
    if (context == null) SemanticError("trying to search null context for: " + ident, cls.pos)

    println("findDeclType: searching context: " + context)

    val sym = context.resolve{a : (Symbol,Context) => a._1 match {
      // ensure it was defined before if it's the same context
      case t@DeclSymbol(str, _) => str == ident && (t.pos < cls.pos || t.pos == cls.pos ||
                                                    cls.context != a._2 || !a._2.ordered)
      case _ => false 
    }}

    if (sym.isEmpty) SemanticError("symbol " + ident + " unknown", cls.pos)

    val d = sym.get.asInstanceOf[DeclSymbol]
    val (nt, ncon) = findNew(d.declType)
    (d.declType, nt, ncon)
  }

  def resolveClassType(t : Type, tree : Stmt = null) : BoundClassSymbol = {
    // @todo for now namespaces for types is not supported
    var sym : Option[Symbol] = None
    
    val con : Context = (if (tree == null) BaseContext.context else tree.context)
    println("resolving type: " + t + ", con.sym = " + con.sym)
    var bindings : List[(Term,Term)] = if (con.sym != null && con.sym.isInstanceOf[BoundClassSymbol]) con.sym.asInstanceOf[BoundClassSymbol].bindings else List()
    sym = con.resolve{a : (Symbol,Context) => a._1 match {
      // search for name the same and run unifier to get type bindings
      case cs@ClassSymbol(n1,_) => {
        //println("checking symbol: " + t.full + " vs " + cs.t)
        if (n1 == t.full.getName) { bindings = Unifier(true).unifyTerms(t.full.getTerms, cs.t.getTerms, bindings); true } else false
      }
      case _ => false
    }}

    if (sym.isEmpty) SemanticError("could not resolve type " + t, t.pos)

    sym.get match {
      case t@ClassSymbol(_, _) => BoundClassSymbol(t, bindings)
    }
  }

  def classesEqual(cls1 : BoundClassSymbol, cls2 : BoundClassSymbol) : Boolean = {
    true
  }
}
