package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ArrayBuffer,ListBuffer,HashMap,Set}

class CodeGen(tree : Stmt,
              outcl : String => Unit,
              out : String => Unit,
              pre : String => Unit,
              precl : String => Unit) {
  import BaseContext.verbose

  val systemTypes : HashMap[String,String] = HashMap()
  val instToGen : Set[Term] = Set()
  val completedGen : Set[Term] = Set()

  val instDefToGen : Set[(DefStmt,List[(Term,Term)])] = Set()
  val completedDefGen : Set[(DefStmt,List[(Term,Term)])] = Set()

  def start() {
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt] && cls.asInstanceOf[ClassStmt].generic.length == 0
    def filterOuterDef(cls : Stmt) = cls.isInstanceOf[DefStmt] && cls.asInstanceOf[DefStmt].enclosingClass == null

    /*
     * First traverse system classes with a gen method to determine
     * the base-line bound types
     */
    new StmtVisitor(tree, filterClass, collectSystemTypes)
    //println("found system types = " + systemTypes);

    new StmtVisitor(tree, filterClass, genClassAll(List(),ListBuffer()) _)

    new StmtVisitor(tree, filterOuterDef, genDefs(false,List()) _)

    //println("instToGen = " + instToGen);

    // iteratively instantiate class and function generics, which may
    // create more to generate
    while ((instToGen &~ completedGen).size > 0 ||
           (instDefToGen &~ completedDefGen).size > 0) {
      while ((instToGen &~ completedGen).size > 0) {
        val curSet = (instToGen &~ completedGen).clone()
        for (i <- curSet) {
          val styp = Checker.resolveSingleClassType(Type(i), null, null)
          genClassAll(styp.bindings,ListBuffer())(styp.cs.stmt)
          completedGen += i
        }
      }
      while ((instDefToGen &~ completedDefGen).size > 0) {
        val curSet = (instDefToGen &~ completedDefGen).clone()
        for (tt@(ds,b) <- curSet) {
          genDefs(true,b)(ds)
          completedDefGen += tt
        }
      }
    }

    outln("int main(int argc, char* argv[]) { _def_base_main(); return 0; }")
  }

  var seed : Int = 0
  def genImm() : String = {
    seed += 1
    "_gen_" + seed
  }

  val clsInstVOI : HashMap[String,Int] = HashMap()
  var voi : Int = 0
  def getNextVOI() : Int = { voi += 1; voi }

  var tabs : Int = 0
  def tab() { tabs += 1 }
  def untab() { tabs -= 1 }

  def outclinit(s : String) = outcl((List.fill(tabs)("  ").foldRight("")(_+_)) + s)
  def outclln(s : String) = { outclinit(s); outcl("\n") }
  def outinit(s : String) = out((List.fill(tabs)("  ").foldRight("")(_+_)) + s)
  def outln(s : String) = { outinit(s); out("\n") }
  def preinit(s : String) = pre((List.fill(tabs)("  ").foldRight("")(_+_)) + s)
  def preln(s : String) = { preinit(s); pre("\n") }
  def preclinit(s : String) = precl((List.fill(tabs)("  ").foldRight("")(_+_)) + s)
  def preclln(s : String) = { preclinit(s); precl("\n") }
  def outlnbb(s : String) = { outln(s); pre(s + " ") }

  def genCondGoto() = genImm() + "_condition"
  def genBodyGoto() = genImm() + "_body"
  def genEpiGoto() = genImm() + "_epi"
  def genClassName(t : ClassStmt, b : List[(Term,Term)]) = genType(Some(t.getType()), b, true)
  def genDeclName(n : String, cl : String) = "_decl_" + "_" + cl + "_" + n
  def genDeclName(n : String) = "_decl_" + n
  def genDefNameClass(t : ClassStmt, b : List[(Term,Term)], n : String) = "_def_" + genClassName(t,b) + "_" + n
  def genDefNameBase(n : String) = "_def_base_" + n
  def genDefNameBaseCons(t : ClassStmt, b : List[(Term,Term)],n : String) = "_def_cons" + genClassName(t,b) + "_" + n
  def genDefNameBaseConsNew(t : ClassStmt, b : List[(Term,Term)],n : String) = "_def_cons_new" + genClassName(t,b) + "_" + n
  def genFunNameGens(x : Fun) = x.n + "_" + x.terms.map{genTerm(_)}.foldRight("_")(_+_)
  def genInner(tree : List[Stmt], b : List[(Term,Term)], fun : Stmt => Boolean,
               db : ListBuffer[(String,Option[Type],Expression)]) {
    for (t <- tree) if (fun(t)) genClassAll(b,db)(t)
  }
  def genObjectDefInput = "_OBJECT_"

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
  def genClassAll(b : List[(Term,Term)], dbuf : ListBuffer[(String,Option[Type],Expression)])(tree : Stmt) {
    tree match {
      case t@ClassStmt(name, isSystem, generic, parent, lst) => {
        // Ref will be handled specially
        if (!isSystem && /*!t.isAbstract &&*/ name != "Ref") {
          val genName = genClassName(t, b)

          outclln("\n/* output class " + genName + "*/")

          if (!t.isAbstract) clsInstVOI.put(genName,getNextVOI())

          preclln("struct " + genName + ";")
          outclln("struct " + genName + " { /* parent is " + parent + "*/")
          tab()
          if (!parent.isEmpty)
            generateParentSketchRecur(t.context.extensions(0), b, dbuf)
          else
            outclln("int32_t virtual_object_id;")
          genInner(lst, b, _.isInstanceOf[DeclStmt], dbuf)
          untab()
          outclln("};")

          t.declInits = dbuf

          genInner(lst, b, _.isInstanceOf[DefStmt], dbuf)

          outclln("/* END output class " + genName + "*/")
        }
      }
      case t@DeclStmt(mutable,name,typ,expr) => {
        //println("gen decl: " + t + ", b = " + b)
        if (name != "this") {
          val n1 = genDeclName(name, t.enclosingClass.name)
          outclln(genType(typ, b) + " " + n1 + ";")
          if (!expr.isEmpty) dbuf += Tuple3(n1, typ, expr.get)
        }
      }
      case t@DefStmt(_,_,_,_,_) => genDefs(false,b)(t)
      case _ => ;
    }
  }

  def generateParentSketchRecur(ty : SingleType, b : List[(Term,Term)],
                                dbuf : ListBuffer[(String,Option[Type],Expression)]) {
    if (!ty.cs.stmt.parent.isEmpty)
      generateParentSketchRecur(ty.cs.stmt.context.extensions(0), b ++ ty.bindings, dbuf)
    else
      outclln("int32_t virtual_object_id;")
    genInner(ty.cs.stmt.lst, b ++ ty.bindings, _.isInstanceOf[DeclStmt], dbuf)
  }

  // generate defs which will take the class as a parameter if there is one
  def genDefs(doGens : Boolean, binds : List[(Term,Term)])(tree : Stmt) {
    tree match {
      case t@DefStmt(name,gens,nthunks,ret,stmts) => {

        // some special cases that we will handle later
        if ((t.enclosingClass == null ||
            (t.enclosingClass.name != "Ref" &&
             !t.enclosingClass.isAbstract &&
             !t.enclosingClass.isSystem)) &&
          (doGens || gens.length == 0) &&
          !t.isAbstract) {

          val t1 = Fun(name, t.sym.term)
          val subs = Unifier(true).subst(t1, binds)
          val defNameG = if (gens.length > 0) genFunNameGens(subs.asInstanceOf[Fun]) else name

          if (t.enclosingClass != null && !t.isConstructor)
            generateDefFunction(t, genDefNameClass(t.enclosingClass, binds, defNameG), binds, subs)
          else if (t.enclosingClass != null && t.isConstructor) {
            generateDefFunction(t, genDefNameBaseCons(t.enclosingClass, binds, defNameG), binds, subs)
            generateDefFunction(t, genDefNameBaseConsNew(t.enclosingClass, binds, defNameG), binds, subs, true)
          } else {
            generateDefFunction(t, genDefNameBase(defNameG), binds, subs)
          }
        }
      }
      case _ => ;
    }
  }

  def generateDefFunction(t : DefStmt, genName : String,
                          binds : List[(Term,Term)], subs : Term,
                          isHeapCons : Boolean = false) {
    outln("\n/* output function " + genName + "*/")
    var cl : String = if (t.enclosingClass != null) genClassName(t.enclosingClass, binds) else ""

    if (!t.isEntry) {
      if (true) {
        if (!t.isConstructor) {
          outlnbb(genType(t.ret, binds));
        } else {
          if (isHeapCons) outlnbb(cl + "**");
          else outlnbb(cl);
        }
        outlnbb(genName + "(");
        tab()
        if (t.enclosingClass != null && !t.isConstructor) {
          // first parameter is the class
          outlnbb(cl + "* " + genObjectDefInput)
          if (!t.nthunks.isEmpty && t.nthunks.get.length != 0) {
            outinit(",")
            preinit(",")
          }
        }
        // generate input types
        genInputs(t.nthunks, binds)
        untab()
        outlnbb(")");
        pre(";\n");
        outln("{");
        tab()
        // generate body of def
        if (t.enclosingClass == null &&
            (t.name == "exit" || t.name == "exitError" || t.name == "print")) {
              genSpecialDefBody(t.name, subs);
            }
        if (t.isConstructor) {
          outln("/* constructor */")
          if (isHeapCons)
            outln(cl + "* cons = new " + cl + ";");
          else {
            outln(cl + " cons_stack;");
            outln(cl + "* cons = &cons_stack;");
          }

          val voi : Int = clsInstVOI.get(cl).get
          outln("cons->virtual_object_id = " + voi + ";")

          for ((name,typ,expr) <- t.enclosingClass.declInits) {
            val s1 = genExpr(expr,binds)
            //if (typ.get.full.isInstanceOf[Fun] && typ.get.full.asInstanceOf[Fun].n == "Ref") {
            //outln("cons->" + name + " = new " + genType(Some(Type(typ.get.full.getTerms(0))),binds) + ";")
            //} else {
            outln("cons->" + name + " = " + s1 + ";")
            //}
          }

          outln(cl + "* _OBJECT_ = cons;");
        }
        genDefBody(t.stmts, binds)
        if (t.isConstructor) {
          if (isHeapCons) {
            outln(cl + "** cons_ref = new " + cl + "*;");
            outln("*cons_ref = cons;");
            outln("return cons_ref;");
          } else outln("return *cons;");
        }
        untab()
        outln("}");
      }
    } else {
      // if it's an entry method generation is different
    }

    outln("/* END output function " + genName + "*/")
  }

  def genSpecialDefBody(name : String, t1 : Term) {
    name match {
      case "exit" => outln("exit(_decl_i);")
      case "print" => {
        val n : String = t1.asInstanceOf[Fun].terms(0).getName
        n match {
          case "int" => outln("printf(\"%d\\n\",_decl_t);")
        }
      }
      case "exitError" => outln("fprintf(stderr, _decl_s.c_str());")
    }
  }

  def genDefBody(tree : Stmt, binds : List[(Term,Term)]) {
    tree match {
      case StmtList(lst) => lst.foreach{s => genDefBody(s, binds)}
      case t@DeclStmt(mutable,name,typ,expr) => {
        var assign = " /* no assignment */ "
        if (!expr.isEmpty) {
          val outVar = genExpr(expr.get, binds)
          if (BasicTypes.isBasicTerm(Unifier(true).subst(typ.get.full, binds)))
            assign = " = " + outVar
          else
            assign = " = (" + genType(typ, binds) + ")" + outVar
            //assign = " = (" + genType(typ, binds) + ")&(" + outVar + ")"
        }
        outln(genType(typ, binds) + " " + genDeclName(name) + assign + ";")
      }
      case ExprStmt(e) => outln(genExpr(e, binds) + ";")
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
        outln(lexpr + " " + genAssignOP(op) + " " + rexpr + ";")
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
        outln(";")
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
        outln(";")
        untab()
        outln("}")
      }
      case _ => ;
    }
  }

  def travBinary(l : Expression, r : Expression, cur : Expression, op : String, b : List[(Term,Term)]) = {
    val s1 = genExpr(l,b)
    val s2 = genExpr(r,b)
    outputImm(cur, s1, s2, op, b)
  }

  def genRType(t : ResolvedType, clBinds : List[(Term,Term)], noPtr : Boolean = false) : String = {
    t match {
      case SingleType(cs,binds) => genType(Some(Type(cs.t)), binds ++ clBinds, noPtr)
      case _ => "/*<unknown>*/ void"
    }
  }

  def outputImmLiteral(lit : Expression, b : List[(Term,Term)]) : String = {
    lit match {
      case StrLiteral(s) => s
      case NumLiteral(s) => s
      case True() => "true"
      case False() => "false"
      case Null() => "0";
      case _ => ""
    }
  }

  def outputImm(t : Expression, l : String, r : String, op : String, b : List[(Term,Term)]) : String = {
    val ii = genImm()
    outln(genRType(t.sym,b) + " " + ii + " = " + l + " " + op + " " + r + ";")
    ii
  }

  def outputImmStrExpr(id : String, x : Expression, b : List[(Term,Term)]) : String = {
    val ii = genImm()
    if (x.res == null) CodeGenError("expression has no resolution")
    val name = x.res match {
      case Immediate(_,_,_) => genDeclName(id)
      case ClassScope(_,_,_,n,stmt,binds) =>
        (if (x.objectContext != null) x.objectContext + "->" else genObjectDefInput + "->") + genDeclName(id, n)
      case _ => ""
    }
    outln(genRType(x.sym,b) + "& " + ii + " = " + name + ";")
    ii
    //name
  }

  def genFunExpr(expr : Expression, b : List[(Term,Term)], isHeapCons : Boolean) : String = {
    expr match {
      case t@FunExpr(name,gens,param) => {
        var defNameG = name
        var hasGenTerm : Term = null
        var defStmt : DefStmt = null
        if (t.res.s.isInstanceOf[DefSymbol] &&
            t.res.s.asInstanceOf[DefSymbol].hasGens) {
          val t1 = Fun(name, t.res.s.asInstanceOf[DefSymbol].term)
          hasGenTerm = Unifier(true).subst(t1, expr.function_bindings ++ b)
          defNameG = genFunNameGens(hasGenTerm.asInstanceOf[Fun])
          defStmt = t.res.s.asInstanceOf[DefSymbol].stmt
          //outln("Term = " + t.res.s.asInstanceOf[DefSymbol].term)
          //outln("fb = " + expr.function_bindings)
        }
        if (name == "^" && t.res.isInstanceOf[BaseScope]) {
          // this is the special create reference operator
          val p = genExpr(param.get(0), b)
          val ii = genImm()
          outln(genRType(t.sym,b) + " " + ii + " = &(" + p + ");")
          //outln(genRType(t.sym,b) + " " + ii + " = (" + p + ");")
          ii
        } else {
          var ins : List[String] = List()
          var intypes : List[String] = List()
          var initial : String = ""
          if (!param.isEmpty) ins = param.get.map{genExpr(_, b)}
          if (expr.rsym == null) CodeGenError("rsym on funexpr is null?")
          intypes = expr.rsym match {
            case d@DefSymbol(_,_) => d.inTypes.map{genRType(_,expr.function_bindings ++ b)}
            case _ => CodeGenError("other types of rsyms not supported"); List()
          }
          var ingen = (intypes,ins).zipped.map{ "(" + _ + ")" + _ }
          val call = t.res match {
            case Immediate(_,_,_) => genDeclName(name)
            case ClassScope(_,_,_,n,stmt,binds) => {
              val hasComma = if (ins.length > 0) "," else ""
              initial = (if (expr.objectContext != null) expr.objectContext else genObjectDefInput) + hasComma
              if (hasGenTerm != null) instDefToGen += Tuple2(defStmt,expr.function_bindings ++ b)
              if (n == "Ref" && (name == "#" || name == "deref")) {
                initial = (if (expr.objectContext != null) expr.objectContext else genObjectDefInput) + hasComma
                "*"
                //" "
              } else if (n == "Ref" && name == "free") {
                initial = (if (expr.objectContext != null) expr.objectContext else genObjectDefInput) + hasComma
                outln("delete (*" + initial + ");")
                "delete ";
              } else {
                genDefNameClass(stmt, b, defNameG)
              }
            }
            case BaseScope(_,_,_) => {
              if (hasGenTerm != null) instDefToGen += Tuple2(defStmt,expr.function_bindings)
              if (t.isCons && isHeapCons) genDefNameBaseConsNew(t.cons,expr.function_bindings ++ b,defNameG)
              else if (t.isCons) genDefNameBaseCons(t.cons,expr.function_bindings ++ b,defNameG)
              else genDefNameBase(defNameG)
            }
          }
          if (genRType(t.sym,b) == "void") {
            call + "(" + initial + ingen.mkString(",") + ");"
          } else {
            val ii = genImm()
            if (call == "*")
              outln(genRType(t.sym,b) + "& " + ii + " = " + call + "(" + initial + ingen.mkString(",") + ");")
            else if (t.isCons && isHeapCons) {
              outln(genRType(t.sym,b) + "* " + ii + " = " + call + "(" + initial + ingen.mkString(",") + ");")
            } else if (t.isCons && !isHeapCons) {
              val ii2 = genImm()
              outln(genRType(t.sym,b,true) + " " + ii2 + " = " + call + "(" + initial + ingen.mkString(",") + ");")
              outln(genRType(t.sym,b,true) + "* " + ii + " = " + "&(" + ii2 + ");")
            } else {
              outln(genRType(t.sym,b) + " " + ii + " = " + call + "(" + initial + ingen.mkString(",") + ");")
            }
            ii
          }
        }
      }
    }
  }

  def genExpr(expr : Expression, b : List[(Term,Term)]) : String = {
    expr match {
      case t@StrLiteral(_) => outputImmLiteral(t,b)
      case t@NumLiteral(_) => outputImmLiteral(t,b)
      case t@True() => outputImmLiteral(t,b)
      case t@False() => outputImmLiteral(t,b)
      case t@Null() => outputImmLiteral(t,b)
      case t@StrExpr(str) => outputImmStrExpr(str,t,b)
      case t@FunExpr(name,gens,param) => genFunExpr(expr, b, false)
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
      case t@NewExpr(e) => {
        val ii = genImm()
        //outln(genRType(t.sym,b) + " " + ii + " = (" + genRType(t.sym,b) + ")&(" + genFunExpr(e, b, true) + ");")
        outln(genRType(t.sym,b) + " " + ii + " = " + genFunExpr(e, b, true) + ";")
        ii
      }
      case t@NotExpr(e) => {
        val s1 = genExpr(e,b)
        val ii = genImm()
        outln(genRType(t.sym,b) + " " + ii + " = !" + s1 + ";")
        ii
      }
      case t@NegExpr(e) => {
        val s1 = genExpr(e,b)
        val ii = genImm()
        outln(genRType(t.sym,b) + " " + ii + " = -" + s1 + ";")
        ii
      }
      case t@DotExpr(l, r) => {
        if (l.objectContext == null) l.objectContext = t.objectContext
        val s1 = genExpr(l,t.sym.getBindings() ++ b)
        r.sym = t.sym
        r.objectContext = s1
        genExpr(r,l.sym.getBindings() ++ b)
      }
    }
  }

  def genInputs(ins : Option[List[TypeParam]], b : List[(Term,Term)]) {
    ins match {
      case Some(lst) => {
        if (lst.length != 0) {
          outlnbb(genTypeParam(lst.head, b))
          lst.takeRight(lst.length-1).foreach{ty =>
            outlnbb("," + genTypeParam(ty, b))
          }
        }
      }
      case _ => ;
    }
  }

  def genTypeParam(tp : TypeParam, b : List[(Term,Term)]) : String =
    genType(Some(tp.typ), b) + " " + genDeclName(tp.name)

  def genType(typ : Option[Type], b : List[(Term,Term)], noPtr : Boolean = false) : String = {
    typ match {
      case Some(Type(x)) => genTermOuter(Unifier(true).subst(x, b), noPtr)
      case _ => "void"
    }
  }

  def genTermOuter(t : Term, noPtr : Boolean) : String = {
    t match {
      case f@Fun(n, terms) if (n == "Ref") => genTerm(terms(0)) + "**"
      case _ => if (BasicTypes.isBasicTerm(t) || noPtr) genTerm(t) else genTerm(t) + "*"
    }
  }

  def genTerm(t : Term) : String = {
    t match {
      case Bound(x) => if (systemTypes.get(x).isEmpty) "_concrete_" + x else systemTypes.get(x).get
      case f@Fun(n, terms) => addLst(f); "_concrete_" + n + "_" + terms.map{genTerm(_)}.foldRight("_")(_+_)
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
