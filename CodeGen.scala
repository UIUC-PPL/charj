package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ArrayBuffer,ListBuffer,HashMap}

class CodeGen(tree : Stmt, out : String => Unit) {
  import BaseContext.verbose

  val systemTypes : HashMap[String,String] = HashMap()

  def start() {
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt]

    /*
     * First traverse system classes with a gen method to determine
     * the base-line bound types
     */
    new StmtVisitor(tree, filterClass, collectSystemTypes)
    println("found system types = " + systemTypes);

    new StmtVisitor(tree, filterClass, genClassAll)

    def filterDefs(cls : Stmt) = cls.isInstanceOf[DefStmt]
    new StmtVisitor(tree, filterDefs, genDefs)
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

  def genClassName(n : String) = "__concrete_" + n
  def genDeclName(n : String, m : Boolean) = (if (m) "__var_" else "__val_") + n
  def genDefName(cl : String, n : String) = "__def_" + cl + "_" + n
  def genInner(tree : List[Stmt]) { for (t <- tree) genClassAll(t) }
  def genObjectDefInput = "__OBJECT_"

  /*
   *
   * @ eric: I've started to sketch this out a bit more to make it
   * clearer what generating code will entail.
   * 
   */

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

  // generate the class wrapper and everything and belongs inside the
  // actual generated class structure
  def genClassAll(tree : Stmt) {
    tree match {
      case t@ClassStmt(name, isSystem, generic, parent, lst) => {
        // Ref will be handled specially
        if (generic.length == 0 && !isSystem && !t.isAbstract && name != "Ref") {
          val genName = genClassName(name)

          outln("\n/* output class " + genName + "*/")

          outln("struct " + genName + " { /* parent is " + parent + "*/")
          tab()
          genInner(lst)
          untab()
          outln("};")

          outln("/* END output class " + genName + "*/")
        }
      }
      case t@DeclStmt(mutable,name,typ,expr) => {
        if (name != "this")
          outln(genType(typ, false) + " " + genDeclName(name, mutable) + ";")
      }
      case _ => ;
    }
  }

  // generate defs which will take the class as a parameter if there is one
  def genDefs(tree : Stmt) {
    tree match {
      case t@DefStmt(name,gens,nthunks,ret,stmts) => {
        var cl : String = ""
        if (t.enclosingClass != null)
          cl = genClassName(t.enclosingClass.name)
        val genName = genDefName(cl, name)

        // some special cases that we will handle later
        if (t.enclosingClass != null &&
            t.enclosingClass.name != "Ref" &&
            !t.enclosingClass.isAbstract &&
            !t.enclosingClass.isSystem) {

          outln("\n/* output function " + genName + "*/")

          if (!t.isEntry) {
            if (!t.isConstructor) {
              outln(genType(ret, false));
              outln(genName + "(");
              tab()
              if (t.enclosingClass != null) {
                // first parameter is the class
                outln(genClassName(t.enclosingClass.name) + "* " + genObjectDefInput)
                if (!nthunks.isEmpty) outinit(",")
              }
              // generate input types
              genInputs(nthunks, false)
              untab()
              outln(")");
              outln("{");
              tab()
              // generate body of def
              genDefBody(stmts)
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

  def genDefBody(tree : Stmt) {
    tree match {
      case StmtList(lst) => lst.foreach{s => genDefBody(s)}
      case t@DeclStmt(mutable,name,typ,expr) => {
        var assign = " /* no assignment */ "
        if (!expr.isEmpty) {
          val outVar = genExpr(expr.get, tree)
          assign = " = " + outVar
        }
        outln(genType(typ, false) + " " + genDeclName(name, mutable) + assign + ";")
      }
      case t@IfStmt(cond,stmt1,ostmt2) => {
        val outCond = genExpr(cond, tree)
        outln("if (" + outCond + ") {")
        tab()
        genDefBody(stmt1)
        untab()
        outln("}")
        if (!ostmt2.isEmpty) {
          outln("else {")
          tab()
          genDefBody(ostmt2.get)
          untab()
          outln("}")
        }
      }
      case _ => ;
    }
  }

  def genExpr(expr : Expression, tree : Stmt) : String = {
    val ii = traverseExpr(expr)
    ii
  }

  def travBinary(l : Expression, r : Expression, cur : Expression, op : String) = {
    val s1 = traverseExpr(l)
    val s2 = traverseExpr(r)
    outputImm(cur, s1, s2, op)
  }

  def genRType(t : ResolvedType) : String = {
    t match {
      case SingleType(cs,_) => genType(Some(Type(cs.t)), false)
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
      case Null() => outln(genRType(lit.sym) + " " +  ii + " = null;")
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
    val ii = genImm()
    outln(genRType(x.sym) + " " + ii + " = "  + ";")
    ii
  }

  def traverseExpr(expr : Expression) : String = {
    expr match {
      case t@StrLiteral(_) => outputImmLiteral(t)
      case t@NumLiteral(_) => outputImmLiteral(t)
      case t@True() => outputImmLiteral(t)
      case t@False() => outputImmLiteral(t)
      case t@Null() => outputImmLiteral(t)
      case t@StrExpr(head::_) => outputImmStrExpr(head, t)
      case MulExpr(l, r) => travBinary(l, r, expr, "*")
      case DivExpr(l, r) => travBinary(l, r, expr, "/")
      case ModExpr(l, r) => travBinary(l, r, expr, "%")
      case AddExpr(l, r) => travBinary(l, r, expr, "+")
      case SubExpr(l, r) => travBinary(l, r, expr, "-")
      case OrrExpr(l, r) => travBinary(l, r, expr, "||")
      case AndExpr(l, r) => travBinary(l, r, expr, "&&")
      case ComExpr(l, r) => travBinary(l, r, expr, "==")
      case LesExpr(l, r) => travBinary(l, r, expr, "<")
      case LeqExpr(l, r) => travBinary(l, r, expr, "<=")
      case GesExpr(l, r) => travBinary(l, r, expr, ">")
      case GeqExpr(l, r) => travBinary(l, r, expr, ">=")
      case NeqExpr(l, r) => travBinary(l, r, expr, "!=")
      case _ => ""
    }
  }

  def genInputs(ins : Option[List[TypeParam]], isRefType : Boolean) {
    ins match {
      case Some(lst) => {
        if (lst.length != 0) {
          outln(genTypeParam(lst.head, isRefType))
          lst.takeRight(lst.length-1).foreach{ty =>
            outln("," + genTypeParam(ty, isRefType))
          }
        }
      }
      case _ => ;
    }
  }

  def genTypeParam(tp : TypeParam, isRefType : Boolean) : String =
    genType(Some(tp.typ), isRefType) + " " + genDeclName(tp.name, true)

  def genType(typ : Option[Type], isRefType : Boolean) : String = {
    typ match {
      case Some(x@Type(Bound(t))) => systemTypes.get(t).get
      case _ => "void"
    }
  }
}
