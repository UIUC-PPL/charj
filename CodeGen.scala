package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ArrayBuffer,ListBuffer,HashMap,Set}

class CodeGen(tree : Stmt, out : String => Unit) {
  import BaseContext.verbose

  val systemTypes : HashMap[String,String] = HashMap()
  val instToGen : Set[Term] = Set()
  val completedGen : Set[Term] = Set()

  val instDefToGen : Set[Term] = Set()
  val completedDefGen : Set[Term] = Set()

  def start() {
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt] && cls.asInstanceOf[ClassStmt].generic.length == 0
    def filterOuterDef(cls : Stmt) = cls.isInstanceOf[DefStmt] && cls.asInstanceOf[DefStmt].enclosingClass == null

    /*
     * First traverse system classes with a gen method to determine
     * the base-line bound types
     */
    new StmtVisitor(tree, filterClass, collectSystemTypes)
    println("found system types = " + systemTypes);

    new StmtVisitor(tree, filterClass, genClassAll(List()) _)

    new StmtVisitor(tree, filterOuterDef, genDefs(List()) _)

    println("instToGen = " + instToGen);

    while ((instToGen &~ completedGen).size > 0) {
      val curSet = (instToGen &~ completedGen).clone()
      for (i <- curSet) {
        val styp = Checker.resolveSingleClassType(Type(i), null, null)
        genClassAll(styp.bindings)(styp.cs.stmt)
        completedGen += i
      }
    }
  }

  var seed : Int = 0
  def genImm() : String = {
    seed += 1
    "_gen_" + seed
  }
  var tabs : Int = 0
  def tab() { tabs += 1 }
  def untab() { tabs -= 1 }
  def outinit(s : String) = out((List.fill(tabs)("  ").foldRight("")(_+_)) + s)
  def outln(s : String) = {
    outinit(s)
    out("\n")
  }

  def genCondGoto() = genImm() + "_condition"
  def genBodyGoto() = genImm() + "_body"
  def genEpiGoto() = genImm() + "_epi"
  def genClassName(t : ClassStmt, b : List[(Term,Term)]) = genType(Some(t.getType()), b)
  def genDeclName(n : String, cl : String) = "__decl_" + "_" + cl + "_" + n
  def genDeclName(n : String) = "__decl_" + n
  def genDefNameClass(t : ClassStmt, b : List[(Term,Term)], n : String) = "__def_" + genClassName(t,b) + "_" + n
  def genDefNameBase(n : String) = "__def_base_" + n
  def genFunNameGens(x : Fun) = x.n + "_" + x.terms.map{genTerm(_)}.foldRight("___")(_+_)
  def genInner(tree : List[Stmt], b : List[(Term,Term)], fun : Stmt => Boolean) {
    for (t <- tree) if (fun(t)) genClassAll(b)(t)
  }
  def genObjectDefInput = "__OBJECT_"

  def collectSystemTypes(tree : Stmt) {
    tree match {
      case t@ClassStmt(cname,isSystem,_,_,lst) if (isSystem) => {
        for (stmt <- lst) {
          stmt match {
            case DefStmt(name,_,_,_,StmtList(lst2)) if (name == "gen") => {
              for (l <- lst2) {
                l match {
                  case ReturnStmt(Some(StrLiteral(str))) => systemTypes.put(cname,str)
                  case _ => ;
                }
              }
            }
            case _ => ;
          }
        }
      }
      case _ => ;
    }
  }

  /*
   * generate the class wrapper and everything and belongs inside the
   * actual generated class structure
   * 
   * at this point, the structure should be completely well-defined
   * with no free variables
   */
  def genClassAll(b : List[(Term,Term)])(tree : Stmt) {
    tree match {
      case t@ClassStmt(name, isSystem, generic, parent, lst) => {
        // Ref will be handled specially
        if (!isSystem && !t.isAbstract && name != "Ref") {
          val genName = genClassName(t, b)

          outln("\n/* output class " + genName + "*/")

          outln("struct " + genName + " { /* parent is " + parent + "*/")
          tab()
          if (!parent.isEmpty)
            generateParentSketchRecur(t.context.extensions(0), b)
          else
            outln("int32_t virtual_object_id = 0;")
          genInner(lst, b, _.isInstanceOf[DeclStmt])
          untab()
          outln("};")

          genInner(lst, b, _.isInstanceOf[DefStmt])

          outln("/* END output class " + genName + "*/")
        }
      }
      case t@DeclStmt(mutable,name,typ,expr) => {
        //println("gen decl: " + t + ", b = " + b)
        if (name != "this")
          outln(genType(typ, b) + " " + genDeclName(name, t.enclosingClass.name) + ";")
      }
      case t@DefStmt(_,_,_,_,_) => genDefs(b)(t)
      case _ => ;
    }
  }

  def generateParentSketchRecur(ty : SingleType, b : List[(Term,Term)]) {
    if (!ty.cs.stmt.parent.isEmpty)
      generateParentSketchRecur(ty.cs.stmt.context.extensions(0), b ++ ty.bindings)
    else
      outln("int32_t virtual_object_id = 0;")
    genInner(ty.cs.stmt.lst, b ++ ty.bindings, _.isInstanceOf[DeclStmt])
  }

  // generate defs which will take the class as a parameter if there is one
  def genDefs(binds : List[(Term,Term)])(tree : Stmt) {
    tree match {
      case t@DefStmt(name,gens,nthunks,ret,stmts) => {
        var cl : String = ""
        if (t.enclosingClass != null)
          cl = genClassName(t.enclosingClass, binds)
        val genName = if (t.enclosingClass != null) genDefNameClass(t.enclosingClass, binds, name) else genDefNameBase(name)

        // some special cases that we will handle later
        if ((t.enclosingClass == null ||
            (t.enclosingClass.name != "Ref" &&
             !t.enclosingClass.isAbstract &&
             !t.enclosingClass.isSystem)) &&
          gens.length == 0) {

          outln("\n/* output function " + genName + "*/")

          if (!t.isEntry) {
            if (!t.isConstructor) {
              outln(genType(ret, binds));
              outln(genName + "(");
              tab()
              if (t.enclosingClass != null) {
                // first parameter is the class
                outln(cl + "* " + genObjectDefInput)
                if (!nthunks.isEmpty && nthunks.get.length != 0) outinit(",")
              }
              // generate input types
              genInputs(nthunks, binds)
              untab()
              outln(")");
              outln("{");
              tab()
              // generate body of def
              if (t.enclosingClass == null &&
                  (name == "exit" || name == "exitError")) {
                genSpecialDefBody(name);
              }
              genDefBody(stmts, binds)
              untab()
              outln("}");
            } else {
              // non-entry constructor
            }
          } else {
            // if it's an entry method generation is different
          }

          outln("/* END output function " + genName + "*/")
        }

      }
      case _ => ;
    }
  }

  def genSpecialDefBody(name : String) {
    name match {
      case "exit" => outln("exit(__var_i);")
      case "exitError" => outln("fprintf(stderr, __var_s.c_str());")
    }
  }

  def genDefBody(tree : Stmt, binds : List[(Term,Term)]) {
    tree match {
      case StmtList(lst) => lst.foreach{s => genDefBody(s, binds)}
      case t@DeclStmt(mutable,name,typ,expr) => {
        var assign = " /* no assignment */ "
        if (!expr.isEmpty) {
          val outVar = genExpr(expr.get, binds)
          assign = " = " + outVar
        }
        outln(genType(typ, binds) + " " + genDeclName(name) + assign + ";")
      }
      case ExprStmt(e) => outln(genExpr(e, binds))
      case t@IfStmt(cond,stmt1,ostmt2) => {
        val outCond = genExpr(cond, binds)
        outln("if (" + outCond + ") {")
        tab()
        genDefBody(stmt1, binds)
        untab()
        outln("}")
        if (!ostmt2.isEmpty) {
          outln("else {")
          tab()
          genDefBody(ostmt2.get, binds)
          untab()
          outln("}")
        }
      }
      case t@AssignStmt(lval,op,rval) => {
        val lexpr = genExpr(lval, binds)
        val rexpr = genExpr(rval, binds)
        outln(lexpr + " " + genAssignOP(op) + " " + rexpr)
      }
      case t@WhileStmt(expr,stmt) => {
        val condGoto = genCondGoto()
        val bodyGoto = genBodyGoto()
        val epiGoto = genEpiGoto()

        outln(condGoto + ":")

        outln("{");
        tab()
        val outCond = genExpr(expr, binds)
        outln("if (" + outCond + ") goto " + bodyGoto + ";")
        outln("else goto " + epiGoto + ";")
        untab();
        outln("}")

        outln(bodyGoto + ":")
        outln("{");
        tab()
        genDefBody(stmt, binds)
        outln("goto " + condGoto  + ";")
        untab();
        outln("}")

        outln(epiGoto + ":")
      }
      case t@ReturnStmt(expr) => {
        if (expr.isEmpty) {
          outln("return;")
        } else {
          val outCond = genExpr(expr.get, binds)
          outln("return " + outCond + ";")
        }
      }
      case t@ForStmt(decls, expr1, cont, stmt) => {
        outln("{")
        tab()

        for (d <- decls) {
          d match {
            case t@DeclStmt(mutable,name,typ,expr) => {
              var assign = " /* no assignment */ "
              if (!expr.isEmpty) assign = " = " + genExpr(expr.get, binds)
              outln(genType(typ, binds) + " " +  genDeclName(name) + assign + ";")
            }
          }
        }

        val condGoto = genCondGoto()
        outln(condGoto + ":")

        val bodyGoto = genBodyGoto()
        val epiGoto = genEpiGoto()
        if (expr1.isEmpty) 
          outln("goto " + bodyGoto + ";")
        else {
          outln("{");
          tab()
          val condExpr = genExpr(expr1.get, binds)
          outln("if (" + condExpr + ") goto " + bodyGoto + ";")
          outln("else goto " + epiGoto + ";")
          untab();
          outln("}")
        }

        outln(bodyGoto + ":")
        outln("{");
        tab()
        genDefBody(stmt, binds)
        for (x <- cont) genDefBody(x, binds)
        outln("goto " + condGoto  + ";")
        untab();
        outln("}")

        outln(epiGoto + ":")
        untab()
        outln("}")
      }
      case _ => ;
    }
  }

  def travBinary(l : Expression, r : Expression, cur : Expression, op : String, b : List[(Term,Term)]) = {
    val s1 = genExpr(l,b)
    val s2 = genExpr(r,b)
    outputImm(cur, s1, s2, op)
  }

  def genRType(t : ResolvedType) : String = {
    t match {
      case SingleType(cs,binds) => genType(Some(Type(cs.t)), binds)
      case _ => "/*<unknown>*/ void"
    }
  }

  def outputImmLiteral(lit : Expression) : String = {
    val ii = genImm()
    lit match {
      case StrLiteral(s) => outln(genRType(lit.sym) + " " +  ii + " = " + s + ";")
      case NumLiteral(s) => outln(genRType(lit.sym) + " " +  ii + " = " + s + ";")
      case True() => outln(genRType(lit.sym) + " " +  ii + " = true ;")
      case False() => outln(genRType(lit.sym) + " " +  ii + " = false ;")
      case Null() => outln("void* " +  ii + " = null;")
      case _ => ""
    }
    ii
  }

  def outputImm(t : Expression, l : String, r : String, op : String) : String = {
    val ii = genImm()
    outln(genRType(t.sym) + " " + ii + " = " + l + " " + op + " " + r + ";")
    ii
  }

  def outputImmStrExpr(id : String, x : Expression) : String = {
    //val ii = genImm()
    if (x.res == null) CodeGenError("expression has no resolution")
    val name = x.res match {
      case Immediate(_,_,_) => genDeclName(id)
      case ClassScope(_,_,_,n,stmt,binds) => genObjectDefInput + "->" + genDeclName(id, n)
      case _ => ""
    }
    //outln(genRType(x.sym) + "& " + ii + " = " + name + ";")
    //ii
    name
  }

  def genExpr(expr : Expression, b : List[(Term,Term)]) : String = {
    expr match {
      case t@StrLiteral(_) => outputImmLiteral(t)
      case t@NumLiteral(_) => outputImmLiteral(t)
      case t@True() => outputImmLiteral(t)
      case t@False() => outputImmLiteral(t)
      case t@Null() => outputImmLiteral(t)
      case t@StrExpr(str) => outputImmStrExpr(str, t)
      case t@FunExpr(name,gens,param) => {
        var defNameG = name
        if (t.res.s.isInstanceOf[DefSymbol] &&
            t.res.s.asInstanceOf[DefSymbol].hasGens) {
          val t1 = Fun(name, t.res.s.asInstanceOf[DefSymbol].term)
          val subs = Unifier(true).subst(t1, expr.function_bindings)
          defNameG = genFunNameGens(subs.asInstanceOf[Fun])
          //outln("Term = " + t.res.s.asInstanceOf[DefSymbol].term)
          //outln("fb = " + expr.function_bindings)
        }
        if (name == "^" && t.res.isInstanceOf[BaseScope]) {
          // this is the special create reference operator
          val p = genExpr(param.get(0), b)
          val ii = genImm()
          outln(genRType(t.sym) + " " + ii + " = &(" + p + ");")
          ii
        } else {
          var ins : List[String] = List()
          var initial : String = ""
          if (!param.isEmpty) ins = param.get.map{genExpr(_, b)}
          val call = t.res match {
            case Immediate(_,_,_) => genDeclName(name)
            case ClassScope(_,_,_,n,stmt,binds) => {
              initial = genObjectDefInput + ","
              genDefNameClass(stmt, binds, defNameG)
            }
            case BaseScope(_,_,_) => genDefNameBase(defNameG)
          }
          call + "(" + initial + ins.mkString(",") + ")"
        }
      }
      case t@DefExpr(d) => {
        // generate a closure here
        ""
      }
      case MulExpr(l, r) => travBinary(l, r, expr, "*", b)
      case DivExpr(l, r) => travBinary(l, r, expr, "/", b)
      case ModExpr(l, r) => travBinary(l, r, expr, "%", b)
      case AddExpr(l, r) => travBinary(l, r, expr, "+", b)
      case SubExpr(l, r) => travBinary(l, r, expr, "-", b)
      case OrrExpr(l, r) => travBinary(l, r, expr, "||", b)
      case AndExpr(l, r) => travBinary(l, r, expr, "&&", b)
      case ComExpr(l, r) => travBinary(l, r, expr, "==", b)
      case LesExpr(l, r) => travBinary(l, r, expr, "<", b)
      case LeqExpr(l, r) => travBinary(l, r, expr, "<=", b)
      case GesExpr(l, r) => travBinary(l, r, expr, ">", b)
      case GeqExpr(l, r) => travBinary(l, r, expr, ">=", b)
      case NeqExpr(l, r) => travBinary(l, r, expr, "!=", b)
      case t@NotExpr(e) => {
        val s1 = genExpr(e,b)
        val ii = genImm()
        outln(genRType(t.sym) + " " + ii + " = !" + s1 + ";")
        ii
      }
      case t@NegExpr(e) => {
        val s1 = genExpr(e,b)
        val ii = genImm()
        outln(genRType(t.sym) + " " + ii + " = -" + s1 + ";")
        ii
      }
      case _ => ""
    }
  }

  def genInputs(ins : Option[List[TypeParam]], b : List[(Term,Term)]) {
    ins match {
      case Some(lst) => {
        if (lst.length != 0) {
          outln(genTypeParam(lst.head, b))
          lst.takeRight(lst.length-1).foreach{ty =>
            outln("," + genTypeParam(ty, b))
          }
        }
      }
      case _ => ;
    }
  }

  def genTypeParam(tp : TypeParam, b : List[(Term,Term)]) : String =
    genType(Some(tp.typ), b) + " " + genDeclName(tp.name)

  def genType(typ : Option[Type], b : List[(Term,Term)]) : String = {
    typ match {
      case Some(Type(x)) => genTermOuter(Unifier(true).subst(x, b))
      case _ => "void"
    }
  }

  def genTermOuter(t : Term) : String = {
    t match {
      case f@Fun(n, terms) if (n == "Ref") => genTerm(terms(0)) + "*"
      case _ => genTerm(t)
    }
  }

  def genTerm(t : Term) : String = {
    t match {
      case Bound(x) => if (systemTypes.get(x).isEmpty) "__concrete_" + x else systemTypes.get(x).get
      case f@Fun(n, terms) => addLst(f); "__concrete_" + n + "_" + terms.map{genTerm(_)}.foldRight("___")(_+_)
      case t@Thunker(_) => "void*"
      case _ => CodeGenError("could not generate type: " + t); ""
    }
  }

  def addLst(t : Term) = instToGen += t

  def genAssignOP(op : AssignOp) = op match {
    case Equal() => "="
    case PEqual() => "+="
    case MEqual() => "-="
  }
}
