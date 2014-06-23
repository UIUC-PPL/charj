package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ArrayBuffer,ListBuffer}

/*
 * Todos:
 * chare arrays, sections, etc.
 * how to communicate with other languages (extern/etc.)
 * 
 * Completed todos:
 * wait statements
 * first-class functions
 * arbitrary operators
 * fix parsing problem with multiple functions
 * 'this' for class self reference
 * LCA algorithm for classesEqual inheritance hierarchy traversal
 */

class Checker(tree : Stmt) {
  import Checker._
  import BaseContext.verbose

  def start() = {
    // Print out all the class names and whether they are detected to
    // be abstract or not
    if (verbose) println("###\n### traverse print classes ###\n###")
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt]
    def printClass(cls : Stmt) = if (verbose) println("found class name = " + cls.asInstanceOf[ClassStmt].name +
                                                      ", abstract = " + cls.asInstanceOf[ClassStmt].isAbstract)
    new StmtVisitor(tree, filterClass, printClass);

    // Resolve all free vars, bound, and literals, by searching
    // contexts and replacing unknown "Tok" terms
    if (verbose) println("###\n### traverse resolve free vars ###\n###")
    def filterType(cls : Stmt) = cls.isInstanceOf[Type]
    new StmtVisitor(tree, filterType, findFreeVars);
    new ExprVisitor(tree, findFreeVarsExpr);

    // Traverse all abstract defs and add them to a list for the
    // abstract class that must be implemented to make it concrete
    if (verbose) println("###\n### traverse defs/classes to determine concreteness ###\n###")
    def filterDefs(cls : Stmt) = cls.isInstanceOf[DefStmt]
    new StmtVisitor(tree, filterDefs, abstractSignatures)

    // Find subclasses for each class along with level in inheritance
    // hierarchy
    if (verbose) println("###\n### traverse resolve class type ###\n###")
    new StmtVisitor(tree, filterClass, determineClassType);

    // Print the level for each class
    if (verbose) println("###\n### traverse print class level ###\n###")
    def printClassLevel(cls : Stmt) = println(cls.pos + ": class name = " + cls.asInstanceOf[ClassStmt].name +
                                              ", level = " + cls.asInstanceOf[ClassStmt].sym.level +
                                              ", abstract = " + cls.asInstanceOf[ClassStmt].isAbstract)
    new StmtVisitor(tree, filterClass, printClassLevel);

    // Resolve the type (symbol) for each decl (var or val)
    if (verbose) println("###\n### traverse resolve decl type ###\n###")
    def filterDecls(cls : Stmt) = cls.isInstanceOf[DeclStmt] || cls.isInstanceOf[TypeParam]
    new StmtVisitor(tree, filterDecls, determineDeclType)

    // Resolve the type (symbol) for each def (function/method)
    if (verbose) println("###\n### traverse resolve def type ###\n###")
    def filterFunctions(cls : Stmt) = cls.isInstanceOf[DefStmt] || cls.isInstanceOf[TypeParam]
    new StmtVisitor(tree, filterFunctions, determineDefType)

    // Traverse all expressions and propagate types, check binary
    // operators
    if (verbose) println("###\n### traverse expressions ###\n###")
    new ExprVisitor(tree, determineExprType)

    // Traverse all statements and ensure the types are correct for
    // each input expression
    if (verbose) println("###\n### traverse statements with expressions ###\n###")
    def filterNone(cls : Stmt) = true
    new StmtVisitor(tree, filterNone, checkStmtType)
  }
}

object Checker {
  import BasicTypes._
  import BaseContext.verbose

  def abstractSignatures(tree : Stmt) {
    tree match {
      case t@DefStmt(n,_,_,_,_) if (t.enclosingClass != null && t.isAbstract && t.enclosingWait == null) => {
        if (verbose) println(t.enclosingClass.name + ": add abstract def: " + n)
        t.enclosingClass.abstractDefs += t
      }
      case _ => ;
    }
  }

  def checkStmtType(tree : Stmt) {
    tree match {
      case t@AssignStmt(lval, _, rval) => {
        if (verbose) println(t.pos + ": check assign of: rsym = " + rval.sym + ", lsym = " + lval.sym)

        if (!lval.islval) SemanticError("lval of assignment is not allowed", lval.pos)
        if (!lval.isMutable && !t.enclosingDef.isConstructor)
          SemanticError("illegal modification of readonly val", lval.pos)

        if (!ClassEquality.equal(rval.sym, lval.sym) && !rval.sym.isNull)
          SemanticError("tried to assign to different class type: " + t.sym, rval.pos)
      }
      case t@IfStmt(cond, _, _) => {
        if (verbose) println(t.pos + ": check if stmt: cond sym = " + cond.sym)

        if (cond.sym == null)
          SemanticError("if statement condition type not determined", t.pos)
        if (!ClassEquality.equal(cond.sym, resolveClassType(Type(booleanType),tree,tree)))
          SemanticError("if statement condition must be of type boolean", t.pos)
      }
      case t@WhileStmt(expr1, _) => {
        if (expr1.sym == null)
          SemanticError("while statement condition type not determined", t.pos)
        if (!ClassEquality.equal(expr1.sym, resolveClassType(Type(booleanType),tree,tree)))
          SemanticError("while statement condition must be of type boolean", t.pos)
      }
      case t@ForStmt(decls, expr1, cont, stmt) => {
        if (!expr1.isEmpty) {
          if (expr1.get.sym == null)
            SemanticError("for statement condition type not determined", t.pos)
          if (!ClassEquality.equal(expr1.get.sym, resolveClassType(Type(booleanType),tree,tree)))
            SemanticError("for statement condition must be of type boolean", t.pos)
        }
      }
      case t@DeclStmt(_, _, optType, Some(expr1)) => {
        if (verbose) println(t.pos + ": check decl stmt: expr1 sym = " + expr1.sym)

        if (expr1.sym == null)
          SemanticError("decl statement condition type not determined", t.pos)
        if (optType.isEmpty)
          SemanticError("type required on decl", t.pos)
        if (!ClassEquality.equal(expr1.sym, resolveClassType(optType.get,tree,tree)) && !expr1.sym.isNull) {
          val typ = resolveClassType(optType.get,tree,tree)
          SemanticError("decl type and expression must match: " + expr1.sym + " and " + typ, t.pos)
        }
      }
      case t@ReturnStmt(optExp) => {
        if (verbose) println(t.pos + ": check return stmt")
        if (verbose) println("retType = " + t.enclosingDef.sym.retType)

        if (optExp.isEmpty) {
          if (!ClassEquality.equal(t.enclosingDef.sym.retType, resolveClassType(Type(unitType),tree,tree)))
            SemanticError("return type of def must be unit (or not present)", t.pos)
        } else if (optExp.get.sym != null) {
          if (!ClassEquality.equal(t.enclosingDef.sym.retType, optExp.get.sym))
            SemanticError("return type of def must match declared: " + optExp.get.sym, t.pos)
        } else {
          SemanticError("unable to resolve type of return", t.pos)
        }
      }
      case _ => ;
    }
  }

  def findFreeVarsExpr(expr : Expression, cls : Stmt) {
    expr match {
      case t@FunExpr(_,_,_) => {
        if (t.generic != List()) {
          if (verbose) println("resolving generics for " + t.generic)
          t.generic = t.generic.map{checkTerm(_, cls)}
          if (verbose) println("resolved to " + t.generic)
        }
      }
      case t@DefExpr(stmt) => {
        if (!stmt.nthunks.isEmpty) {
          for (tp <- stmt.nthunks.get)
            tp.typ.full = checkTerm(tp.typ.full, cls)
          if (!stmt.ret.isEmpty)
            stmt.ret.get.full = checkTerm(stmt.ret.get.full, cls)
        }
      }
      case _ => ;
    }
  }

  def findFreeVars(tree : Stmt) {
    tree match {
      case t@Type(term) => t.full = checkTerm(term, tree)
    }
  }

  def checkTerm(t : Term, tree : Stmt) : Term = {
    t match {
      case Bound(t) => Bound(t)
      case MVar(t) => MVar(t)
      case Fun(n, terms) => Fun(n, terms.map{checkTerm(_, tree)})
      case Tok(t) => checkTermString(t, tree)
      case Thunker(ts) => Thunker(ts.map{checkTerm(_, tree)})
      case _ => null
    }
  }

  def checkTermString(s : String, tree : Stmt) : Term = {
    var may : Option[SingleType] = None
    var cls : SingleType = null
    val clsName = (if (tree.enclosingClass != null) tree.enclosingClass.name else "")
    val defName = (if (tree.enclosingDef != null) tree.enclosingDef.name else "")

    if (verbose) println("checkTermString: built name to check = " + clsName + "_" + defName + "_" + s)

    if (tree.enclosingClass != null && tree.enclosingDef != null) {
      if (verbose) println("checkTermString: both class and def and non-null")
    }

    may = maybeResolveSingleClass(Type(Tok("fungen_" + defName + "_" + s)), tree, tree)

    if (may.isEmpty) {
      if (tree.enclosingClass != null)
        may = maybeResolveSingleClass(Type(Tok(clsName + "_" + s)), tree, tree)
      if (may.isEmpty) cls = resolveSingleClassType(Type(Tok(s)),tree,tree)
      else cls = may.get
    } else cls = may.get

    val typ : Type = Type(Tok(s))
    if (verbose) println("resolved term string: " + typ + ", cls = " + cls)

    // no substitution needed because these are resolved terminals by definition
    cls.cs.t
  }

  def determineDefType(tree : Stmt) {
    var params : List[Term] = List()
    var returnType : Option[Type] = None
    var context : Stmt = tree
    var isConstructor : Boolean = false
    var sym : DefSymbol = null
    var symname : String = ""

    tree match {
      case t@DefStmt(name,_, maybeParams, hasRet, lst) => {
        symname = name
        params = if (maybeParams.isEmpty) List() else maybeParams.get.map{_.typ.full}
        context = lst
        returnType = hasRet
        isConstructor = t.isConstructor
        sym = t.sym
      }
      case t@TypeParam(name,Type(th@Thunker(lst))) => {
        symname = name
        params = lst.dropRight(1)
        context = t
        returnType = Some(Type(lst.last))
        sym = t.defSym
      }
      case _ => return
    }
    if (verbose) println("resolving type for def: " + tree);

    val inTypes = params.map {tparam =>
      if (verbose) println("\tresolving type for param: " + tparam)
      resolveClassType(Type(tparam),tree,context)
    };
    if (verbose) println("\tresolving type for ret: " + returnType);
    var retType = returnType match {
      case Some(x) => resolveClassType(x,tree,tree)
      case None => resolveClassType(Type(unitType),tree,tree)
    }
    if (isConstructor) retType = resolveClassType(tree.enclosingClass.getType(),tree,tree)

    if (sym == null) {
      SemanticError("could not resolve def types", tree.pos)
    } else {
      sym.inTypes = inTypes
      sym.retType = retType
      sym.declType = FunType(inTypes ++ List(retType))
      if (verbose) println(symname + " def ret is " + retType + ", inTypes = " + inTypes)
    }
  }

  def setChildrenLevelRecur(sym : ClassSymbol) {
    for (child <- sym.children) {
      val cls = resolveSingleClassType(child.stmt.parent.get,child.stmt,child.stmt)
      // compute this classes bindings
      child.parentBindings ++= cls.bindings
      // propagate parent bindings up
      child.parentBindings ++= cls.cs.parentBindings
      child.level = sym.level + 1
      addAbstractDefs(child.stmt, sym.stmt, child.parentBindings)
      setChildrenLevelRecur(child)
    }
  }

  def checkDefsMatch(abs : DefStmt, t : DefStmt, bindings : ListBuffer[(Term,Term)]) = {
    if (abs.name == t.name &&
        abs.gens.length == t.gens.length) {
      println("checking if defs match: " + abs + ", " + t)
      var matchingInputs = true
      var matchingOutput = true
      
      // check each param for matching types
      (abs.nthunks,t.nthunks) match {
        case (Some(l1),Some(l2)) => {
          if (l1.length == l2.length) {
            for ((p1,p2) <- (l1,l2).zipped.toList) {
              val sub1 = Unifier(false).subst(p1.typ.full, bindings.toList)
              println("\t input: " + p1 + ", " + p2)
              var iseq = Unifier(false).isEqual(sub1, p2.typ.full)
              println("\t input bound: " + sub1 + ", " + p2 + " with binds: " + bindings + " => " + iseq)
              if (!iseq) matchingInputs = false
            }
          } else matchingInputs = false
        }
        case _ => matchingInputs = true
      }

      // check for matching output types
      // @todo what about unit/implicit specification
      println("\t returns: " + abs.ret + ", " + t.ret)
      (abs.ret,t.ret) match {
        case (Some(r1),Some(r2)) => {
          val sub1 = Unifier(false).subst(r1.full, bindings.toList)
          println("\t output: " + r1 + ", " + r2)
          var iseq = Unifier(false).isEqual(sub1, r2.full)
          println("\t output bound: " + sub1 + ", " + r2 + " with binds: " + bindings + " => " + iseq)
          matchingOutput = iseq
        }
        case (None,None) => matchingOutput = true
        case _ => matchingOutput = false
      }

      matchingInputs && matchingOutput
    } else false
  }

  def addAbstractDefs(cur : ClassStmt, parent : ClassStmt, bindings : ListBuffer[(Term,Term)]) {
    for (abs <- parent.abstractDefs) {
      var foundMatch = false

      def checkDef(tree : Stmt) {
        tree match {
          case t@DefStmt(_,_,_,_,_) => {
            if (checkDefsMatch(abs, t, bindings)) foundMatch = true
          }
        }
      }
      new StmtVisitor(cur, _.isInstanceOf[DefStmt], checkDef)
      
      if (!foundMatch) {
        cur.abstractDefs += abs
        println("&&&&&& " + cur.name + " detected abstract: " + cur.abstractDefs)
        cur.isAbstract = true
        cur.sym.isAbstract = true
      }
    }
  }

  def determineClassType(tree : Stmt) {
    tree.asInstanceOf[ClassStmt] match {
      case t@ClassStmt(name, _, _, Some(parent), _) => {
        if (verbose) println(t.pos + ": name: " + name + ", parent: " + parent + " type is " + t.sym)
        val cls = resolveSingleClassType(parent,tree,tree)
        if (cls != null) t.context.extensions += cls
        if (verbose) println("\t subclass of " + cls)

        // Compute the levels in the inheritance tree. Traverse up
        // and down the tree as classes are introduced, building the
        // parent-child relationship both ways
        cls.cs.children += t.sym
        if (cls.cs.level != -1) {
          t.sym.level = cls.cs.level + 1
          // add in this classes bindings
          t.sym.parentBindings ++= cls.getBindings()
          // propagate bindings up to parent
          t.sym.parentBindings ++= cls.cs.parentBindings
          addAbstractDefs(t, cls.cs.stmt, t.sym.parentBindings)
        }
      }
      case t@ClassStmt(name, _, _, None, _) => {
        // Propagate level counts up the inheritance tree, once the
        // bottom is reached
        t.sym.level = 1
        setChildrenLevelRecur(t.sym);
      }
      case _ => ;
    }
  }

  def determineDeclType(tree : Stmt) : ResolvedType = {
    var n1 : String = ""
    var t1 : Type = null
    tree match {
      case t@DeclStmt(_, name, Some(typ), _) => { n1 = name; t1 = typ }
      case t@TypeParam(name, typ) => { n1 = name ; t1 = typ }
      case _ => {
        SemanticError("decl types must be explicit currently", tree.pos)
        return null
      }
    }
    if (verbose) println("determineDeclType: " + n1)
    val sym = resolveClassType(t1,tree,tree)
    val thisSym = tree.context.resolve{a : (Symbol,Context) => a._1 match {
      case DeclSymbol(n, _) => n == n1
      case DefSymbol(n, _) => n == n1
      case _ => false
    }}
    if (thisSym == NoSymbol())
      SemanticError(n1 + " not resolved", tree.pos)
    else if (verbose) println("resolved " + n1 + " to " + thisSym.get + " with type: " + sym)

    thisSym.get._1.asInstanceOf[HasDeclType].declType = sym
    sym
  }

  def determineExprType(expr : Expression, cls : Stmt) {
    expr match {
      case FunExpr(name, gens, param) => {
        if (verbose) println("determining type for FunExpr: " + name)
        val exprs = if (param.isEmpty) List() else param.get
        val types : List[ResolvedType] = exprs.map(_.sym)
        val (sym,con) = (expr.sym, if (expr.context == null) cls.context else expr.context)

        if (verbose) println(expr.pos + ": function call: " + name + ", gens = " + gens + ", sym = " + sym)

        val prevBindings = if (sym != null) sym.getBindings() else List()
        var (sym2, term2, con2) = findFunType(expr, name, con, types, gens, prevBindings)

        val (nt, ncon) = findNew(sym2,expr,sym2.getBindings() ++ prevBindings)

        if (verbose) println("FunExpr: found function type: " + sym2 + ", nt = " + nt)

        expr.sym = resolveAnySymbol(Type(nt), expr, null)
        expr.context = ncon
      }
      case StrLiteral(str) => {
        expr.sym = resolveClassType(Type(stringType),expr,cls)
      }
      case AsyncExpr(aexpr) => {
        expr.sym = aexpr.sym
      }
      case SyncExpr(sexpr) => {
        expr.sym = sexpr.sym
      }
      case t@DefExpr(stmt) => {
        expr.sym = stmt.sym.declType
      }
      case NumLiteral(str) => {
        if (str contains ".") {
          println("found double: str = " + str + ", contains = " + (str contains "."))
          expr.sym = resolveClassType(Type(doubleType),expr,cls)
        } else {
          val theInt = tryConvertToInt(str)
          if (!theInt.isEmpty)
            expr.sym = resolveClassType(Type(intType),expr,cls)
        }
      }
      case t@NewExpr(e) => {
        if (e.sym == null)
          SemanticError("could not resolve type of " + e, e.pos)
        e.sym match {
          case SingleType(cs,binds) => {
            val sym2 = resolveAnySymbol(Type(Fun("Ref", List(cs.t))), e, null)
            val (nt, ncon) = findNew(sym2,expr,sym2.getBindings() ++ binds)
            val sym3 = resolveAnySymbol(Type(nt), e, null)
            expr.sym = sym3
            expr.context = ncon
          }
          case _ => SemanticError("can not construct Ref to " + e, e.pos)
        }
        println("resolved newx expr to type: " + expr.sym)
      }
      case t@StrExpr(str) => {
        val (sym,con) = (expr.sym, if (expr.context == null) cls.context else expr.context)
        if (verbose) println("strexpr determine type of: " + str + ", context = " + con)

        if (verbose) println("strexpr push over: " + expr.sym)
        val (sym2,_,_) = findIdentType(t, null, con, List(str), sym)
        val (nt, ncon) = findNew(sym2,expr,sym2.getBindings())
        val sym3 = resolveAnySymbol(Type(nt), expr, null)

        if (verbose) println("new term from strexpr = " + nt)
        if (verbose) println("check type of StrExpr: " + sym3)

        expr.sym = sym3
        expr.context = ncon
      }
      case True() | False() => expr.sym = resolveClassType(Type(booleanType),expr,cls)
      case DotExpr(l, r) => {
        if (l.sym == null) {
          SemanticError("should be resolved to type" + l, l.pos)
        } else {
          if (verbose) println("dotexpr lhs: resolved to type: " + l.sym)
          if (verbose) println("dotexpr rhs: resolved to type: " + r.sym)

          // this is tricky: the visitor will visit the node twice,
          // once before traversing the right, and once after
          // traversing both. the first time we propagate the left to
          // the right for resolution. the second time we set the
          // expression to the right. both of these operations are
          // essentially idempotent
          expr.sym = r.sym
          expr.context = r.context
          r.sym = l.sym
          r.context = l.context
        }
      }
      case AddExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case DivExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case ModExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case SubExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case MulExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case AndExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case OrrExpr(l, r) => checkBinarySet(l, r, expr, l.sym)
      case ComExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType),expr,cls))
      case LesExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType),expr,cls))
      case LeqExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType),expr,cls))
      case GesExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType),expr,cls))
      case GeqExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType),expr,cls))
      case NeqExpr(l, r) => checkBinarySet(l, r, expr, resolveClassType(Type(booleanType),expr,cls))
      case AopExpr(l, r, op) => {
        SemanticError(op + ": arbitrary operators currently unsupported", l.pos)
      }
      case NotExpr(l) => {
        if (!ClassEquality.equal(l.sym, resolveClassType(Type(booleanType),expr,cls)))
          SemanticError("boolean negation incorrect type: " + l.sym, l.pos)
        else expr.sym = l.sym
      }
      case NegExpr(l) => {
        if (!ClassEquality.equal(l.sym, resolveClassType(Type(intType),expr,cls)))
          SemanticError("int negation incorrect type: " + l.sym, l.pos)
        else expr.sym = l.sym
      }
      case Null() => {
        expr.sym = resolveClassType(Type(refType),expr,cls)
        expr.sym.isNull = true
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

  def checkBinarySet(l : Expression, r : Expression, cur : Expression, ret : ResolvedType) {
    if (l.sym.isNull || r.sym.isNull) {
      if (verbose) println("found left or right is Null()")
      if ((r.sym.isNull && BasicTypes.isBasic(l.sym)) ||
          (l.sym.isNull && BasicTypes.isBasic(r.sym)))
        SemanticError("r = " + r.sym + ", l = " + l.sym + ": a literal may not be compared to null", r.pos)
    } else if (!ClassEquality.equal(l.sym, r.sym))
      SemanticError("binary op " + cur + " with different types: " + l.sym + " at " + l.pos +  ", " + r.sym, r.pos)
    if (verbose) println("setting up expr to type: " + ret)
    cur.sym = ret
  }

  def findIdentType(cls : Expression, t : Term, context : Context, n : List[String],
                    sym : ResolvedType) : (ResolvedType, Term, Context) = {
    if (n.size != 0 && verbose) println("recursive findIdentType: " + n + ", context = " + context +
                                        " bindings = " + (if (sym != null) sym.getBindings() else List()))

    if (n.size != 0) {
      val lst = if (sym != null) sym.getBindings() else List()
      val (sym2,term,con) = findDeclType(context, cls, n.head, lst)
      if (verbose) println("\tresolved type for " + n.head + " = " + term + " bcs = " + sym2)
      findIdentType(cls, term, con, n.tail, sym2)
    } else (sym, t, context)
  }

  def findFunType(expr : Expression, methodName : String, context : Context, lst : List[ResolvedType],
                  gens : List[Term], bindings : List[(Term,Term)]) : (ResolvedType, Term, Context) = {
    if (verbose) println("trying to resolve function " + methodName + " bindings = " + bindings)

    var function_bindings : List[(Term, Term)] = List()
    var retType : ResolvedType = null
    var isConstructor : Boolean = false

    //function to check if input parameters match defined types
    def compareTerms(zipped : List[(ResolvedType,ResolvedType)],
                     lbinds : List[(Term,Term)],
                     rbinds : List[(Term,Term)]) : Boolean = {
      var isMatching = true
      for ((t1, t2) <- zipped) {
        if (verbose) println("compareTerms: before: t1 = " + t1 + ", t2 = " + t2 +
                             ", lbinds = " + lbinds + ", rbinds = " + rbinds)
        val u = Unifier(false)
        val sub1 = u.subst(mapToTerm(t1), t1.getBindings() ++ lbinds)
        val sub2 = u.subst(mapToTerm(t2), t2.getBindings() ++ rbinds)
        if (t2.isNull && (sub1.isInstanceOf[Fun] || sub1.isInstanceOf[Thunker])) isMatching = true
        else {
          if (!ClassEquality.equal(t1, t2, lbinds, rbinds, ClassEquality.RHS())) isMatching = false
        }
        if (verbose) println("after: t1 = " + sub1 + ", t2 = " + sub2)
        if (verbose) println("check if terms are equal: " + isMatching)
      }
      isMatching
    }

    val sym = context.resolve{a : (Symbol,Context) => a._1 match {
      case t@DefSymbol(name, _) => {
        if (name == methodName && t.inTypes.size == lst.size) {
          if (verbose) println("findFunType: trying to match DefSymbol(" + name + ")" + " type: " + t.declType)
          // reset function bindings for this possibility
          function_bindings = List()

          if (t.isCons) isConstructor = true
          if (t.isCons && t.classCons.generic != List()) {
            if (verbose) println("\t is constructor def: abstract = " + t.classCons.sym)
            if (t.classCons == null) SemanticError("this is a constructor, should have a class specified", t.pos);
            // if the class constructor being called is abstract this is invalid
            if (t.classCons.sym.isAbstract) SemanticError("can not instantiate abstract class", expr.pos);
            function_bindings = Unifier(true).unifyTerm(t.classCons.getType().full, Fun(name, gens), List())
            if (verbose) println("constructor bindings = " + function_bindings)
          }

          // if it's not a constructor and it has function generics to instantiate
          if (!t.isCons && (gens.length > 0 || t.term.length > 0)) {
            var compGens = gens

            // instantiate with parameter types
            if (gens.length == 0) {
              compGens = lst.map{t1 =>
                if (verbose) println("creating type from input: " + t1)
                t1 match {
                  case SingleType(st,binds) => Unifier(false).subst(st.t, binds)
                  case _ => Tok("")
                }
              }.take(t.term.length)
            }
            val t1 : Term = Fun(methodName, compGens)
            val t2 : Term = Fun(methodName, t.term)
            if (verbose) println("t1 term = " + t1 + ", t2 term = " + t2)
            function_bindings = Unifier(true).unifyTerm(t2,t1,List());
            if (verbose) println("generic function inst bindings = " + function_bindings)
          }

          val toComp = (t.inTypes,lst).zipped.toList
          var isMatching = compareTerms(toComp, function_bindings ++ bindings, List())
          if (isMatching) retType = t.retType
          isMatching
        } else false
      }
      case t@DeclSymbol(name, _) if (name == methodName) => {
        if (verbose) println("findFunType: trying to match DeclSymbol(" + name + ")" + " type: " + t.declType)
        var isMatching = false
        t.declType match {
          case FunType(types) if (types.length - 1 == lst.length) => {
            val toComp = (lst,types.dropRight(1)).zipped.toList
            isMatching = compareTerms(toComp, function_bindings ++ bindings, List())
            if (isMatching) retType = types.last
          }
          case _ => isMatching = false
        }
        isMatching
      }
      case _ => false
    }}

    if (sym.isEmpty)
      SemanticError("could not find matching function for \"" + methodName + "\"", expr.pos)

    println("findfunType: resolved context bindings: " + sym.get._2)

    expr.res = sym.get._3

    // (sym.get._2): propagate the bindings from a subtype when finding the new type
    val (nt, ncon) = findNew(retType,expr,function_bindings ++ bindings ++ sym.get._2)

    if (isConstructor) {
      // it must be a proper def in this case (not a decl bound to a func)
      val d = sym.get._1.asInstanceOf[DefSymbol]
      // unify with class decl to ensure return type of constructor has equal arity
      Unifier(true).unifyTerm(d.classCons.sym.t, nt, List())
    }

    if (verbose) println("resolved def rettype to: " + retType + ", new term = " + nt)

    val sym3 = resolveAnySymbol(Type(nt), expr, null)

    if (verbose) println("resolved final type to: " + sym3)

    (sym3, nt, ncon)
  }

  def mapToTerm(rt : ResolvedType) : Term = {
    rt match {
      case st@SingleType(cs,_) => cs.t
      case ft@FunType(terms) => Thunker(terms.map{b => mapToTerm(b)})
    }
  }

  def findNew(sym : ResolvedType, pos : Positional, bindings : List[(Term,Term)] = List()) : (Term, Context) = {
    if (verbose) println("sym = " + sym + ", binds = " + bindings)
    sym match {
      case sym@SingleType(_,_) => {
        val nt = Unifier(true).subst(sym.cs.t, sym.bindings ++ bindings)
        if (nt == sym.cs.t) (nt, sym.cs.context)
        else {
          val ret = maybeResolveSingleClass(Type(nt), pos, null)
          (nt, if (ret.isEmpty) BaseContext.context else ret.get.cs.context)
        }
      }
      case sym@FunType(lst) => (mapToTerm(sym), BaseContext.context)
    }
  }

  def findDeclType(context : Context, cls : Expression, ident : String,
                   bindings : List[(Term,Term)] = List()) : (ResolvedType, Term, Context) = {
    if (context == null) SemanticError("trying to search null context for: " + ident, cls.pos)

    if (verbose) println("findDeclType: searching context: " + context)

    // ensure it was defined before if it's the same context
    def identity(str : String, t : Positional, c : Context) : Boolean =
      str == ident && (t.pos < cls.pos || t.pos == cls.pos || context != c || !c.ordered)

    val sym = context.resolve{a : (Symbol,Context) => a._1 match {
      case t@DeclSymbol(str, _) => identity(str, t, a._2)
      case t@DefSymbol(str, _) => identity(str, t, a._2)
      case _ => false 
    }}

    if (verbose) println("findDeclType: sym = " + sym);

    if (sym.isEmpty) SemanticError("identifier \"" + ident + "\" can not be found", cls.pos)

    // set the resolution type for the identifier
    cls.res = sym.get._3

    // set whether this is possibly a valid lval
    cls.islval = sym.get._1.isInstanceOf[DeclSymbol]
    cls.isMutable = if (sym.get._1.isInstanceOf[DeclSymbol]) sym.get._1.asInstanceOf[DeclSymbol].isMutable else true

    val d = sym.get._1.asInstanceOf[HasDeclType]
    val (nt, ncon) = findNew(d.declType,cls,sym.get._2 ++ bindings)

    if (verbose) println("findDeclType: d.declType.bindings = " + d.declType.getBindings() + ", sym.get._2 = " + sym.get._2 + ", bindings = " + bindings)
    if (verbose) println("findDeclType: nt = " + nt)

    val sym3 = resolveAnySymbol(Type(nt), cls, null)
    (sym3, nt, ncon)
  }

  def resolveAnySymbol(t : Type, pos : Positional, tree : Stmt) : ResolvedType = {
    t.full match {
      case Thunker(terms) => FunType(terms.map{t : Term => resolveAnySymbol(Type(t), pos, tree)})
      case t@MVar(_) => {
        val cs = ClassSymbol("mvar", 0)
        cs.t = t
        SingleType(cs, List())
      }
      case _ => {
        val opt = maybeResolveSingleClass(t, pos, tree)
        if (opt.isEmpty) SemanticError("could not resolve type: " + t, pos.pos)
        opt.get
      }
    }
  }

  def maybeResolveSingleClass(t : Type, pos : Positional, tree : Stmt) : Option[SingleType] = {
    // @todo for now namespaces for types is not supported
    var sym : Option[(Symbol,List[(Term,Term)],ResolutionType)] = None
    val con : Context = (if (tree == null) BaseContext.context else tree.context)
    var bindings : List[(Term,Term)] = if (con.sym != null && con.sym.isInstanceOf[SingleType]) con.sym.asInstanceOf[SingleType].bindings else List()
    sym = con.resolve{a : (Symbol,Context) => a._1 match {
      // search for name the same and run unifier to get type bindings
      case cs@ClassSymbol(n1,_) => {
        if (n1 == t.full.getName) {
          bindings = Unifier(true).unifyTerms(cs.t.getTerms, t.full.getTerms, bindings); true
        } else false
      }
      case _ => false
    }}

    if (sym.isEmpty) None
    else sym.get._1 match {
      case t@ClassSymbol(_, _) => Some(SingleType(t, bindings ++ sym.get._2))
      case _ => None
    }
  }

  def resolveClassType(t : Type, pos : Positional, tree : Stmt = null) : ResolvedType = {
    resolveAnySymbol(t, pos, tree)
  }

  def resolveSingleClassType(t : Type, pos : Positional, tree : Stmt = null) : SingleType = {
    val ret = maybeResolveSingleClass(t, pos, tree)
    if (ret.isEmpty) {
      SemanticError("could not resolve type " + t.full, pos.pos)
      null
    } else
      ret.get
  }
}
