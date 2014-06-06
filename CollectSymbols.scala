package CharjParser

import scala.util.parsing.input.{Positional,Position}

object BasicTypes {
  val booleanType = Bound("boolean")
  val intType = Bound("int")
  val stringType = Bound("string")
  val charType = Bound("char")
  val unitType = Bound("unit")
  val refType = Fun("Ref", List(MVar("T")))
}

class Collector(tree : Stmt) {
  import BasicTypes._

  def start() = {
    traverseTree(tree, BaseContext.context)
    //BaseContext.context.addInImplicits(null)
  }

  def print(context : Tuple2[Context, Stmt], indent : Int) {
    val str = (0 to indent).map(_ => "\t").foldLeft("")((b,a) => b + a)
    if (context._1 == null) {
      println(str + context._2.getName() + ": no context")
    } else {
      if (context._1.lst.size != 0)
        println(str + context._2.getName() + ":  " + context._1.lst.unzip3._1.toString())
      val (syms, stmts, cons) = context._1.lst.unzip3
      val others = cons zip stmts
      for (child <- others) print(child, indent + 1)
    }
  }

  def newContext(context : Context, stmt : Stmt, isOrdered : Boolean) = new Context(Some(context), isOrdered)

  var enclosingClass : ClassStmt = null

  def traverseTree(tree : Stmt, context : Context) {
    tree.enclosingClass = enclosingClass
    tree.context = context
    tree match {
      case StmtList(lst) => traverseTree(lst, context)
      case t@ClassStmt(name, _, generic, parent, lst) => {
        val arity = generic.size
        val con = newContext(context, tree, false)
        t.generic = generic.map{ gen => {
          val newCon = new Context(None, false)
          val newGen = MVar(name + "_" + gen.asInstanceOf[MVar].t)
          val sym = addClass(con, newGen, newCon, newGen.asInstanceOf[MVar].t, 0, gen.pos, List(), false)
          con.lst += ((sym, tree, newCon))
          newGen
        }}
        // do not use generic past this point
        t.sym = addClass(context, tree, con, name, arity, tree.pos, t.generic, true)
        t.sym.context = con
        con.sym = t.sym
        t.context = con
        enclosingClass = t

        // add in a "this" decl for type checking
        val thisDecl = DeclStmt(false, "this",
                                Some(Type(if (generic.isEmpty) Tok(name) else Fun(name, generic))),
                                Some(Null())
                              )
        thisDecl.pos = t.pos
        // add in the "this" decl
        t.lst = thisDecl::lst

        if (!parent.isEmpty)
          traverseTree(parent.get, con)
        traverseTree(t.lst, con)
        enclosingClass = null
      }
      case t@DefStmt(_, name, gens, nthunks, ret, lst) => {
        val con = newContext(context, tree, true)
        val isAbstract = lst == EmptyStmt()
        val isConstructor = t.enclosingClass != null && t.enclosingClass.name == name
        val arity = if (nthunks.isEmpty) 0 else nthunks.get.length

        val ecn = if (t.enclosingClass != null) t.enclosingClass.name else ""
        val edn = name

        t.gens = gens.map{gen => {
          val newCon = new Context(None, false)
          val newGen = MVar(ecn + "_" + edn + "_" + gen.asInstanceOf[Tok].t)
          val sym = addClass(con, newGen, newCon, newGen.asInstanceOf[MVar].t, 0, gen.pos, List(), false)
          con.lst += ((sym, tree, newCon))
          newGen
        }}

        if (t.enclosingClass != null &&
            t.enclosingClass.name == name) {
          t.sym = addDef(gens, BaseContext.context, tree, con, name, tree.pos, isAbstract, arity)
          t.sym.isCons = true
          t.sym.classCons = enclosingClass
        } else {
          t.sym = addDef(gens, context, tree, con, name, tree.pos, isAbstract, arity)
        }
        con.sym = enclosingClass.sym
        if (t.enclosingClass != null) {
          t.enclosingClass.sym.isAbstract = isAbstract
          t.enclosingClass.isAbstract = isAbstract
          t.sym.isConstructor = isConstructor
          t.isConstructor = isConstructor
        }
        if (!nthunks.isEmpty) {
          for (param <- nthunks.get) {
            param.decl = addDecl(con, tree, null, param.name, param.pos, false)
            param.context = con
            traverseTree(param, con)
          }
        }
        traverseTree(ret, con)
        traverseTree(lst, con)
      }
      case t@TypeParam(_, typ) => traverseTree(typ, context)
      case t@DeclStmt(mutable, name, typ, expr) => {
        if (expr.isEmpty && t.enclosingClass != null) {
          t.enclosingClass.sym.isAbstract = true
          t.enclosingClass.isAbstract = true
        }
        t.sym = addDecl(context, tree, null, name, tree.pos, mutable)
        if (!typ.isEmpty) traverseTree(typ.get, context)
      }
      case ForStmt(decls, _, cont, stmt) => {
        val con = newContext(context, tree, true)
        con.sym = enclosingClass.sym
        traverseTree(decls, con)
        traverseTree(stmt, con)
      }
      case IfStmt(_, stmt1, stmt2) => {
        val con = newContext(context, stmt1, true)
        con.sym = enclosingClass.sym
        traverseTree(stmt1, con)
        if (!stmt2.isEmpty) {
          val con2 = newContext(context, stmt2.get, true)
          con2.sym = enclosingClass.sym
          traverseTree(stmt2, con2)
        }
      }
      case WhileStmt(_, stmt) => {
        val con = newContext(context, stmt, true)
        con.sym = enclosingClass.sym
        traverseTree(stmt, con)
      }
      case _ => ;
    }
  }

  def traverseTree(lst : List[Stmt], context : Context) {
    for (stmt <- lst) traverseTree(stmt, context)
  }

  def traverseTree(mstmt : Option[Stmt], context : Context) {
    mstmt match {
      case Some(stmt) => traverseTree(stmt, context)
      case None => ;
    }
  }

  def addClass(context : Context, stmt : Stmt, newContext : Context,
               name : String, arity : Int, pos : Position, generic : List[Term],
               isBound : Boolean) = {
    val sym = ClassSymbol(name, arity)
    // set up type term for the class symbol
    if (generic == List() && isBound)
      sym.t = Bound(name)
    else if (generic == List() && !isBound) {
      sym.t = MVar(name)
    } else
      sym.t = Fun(name, generic)
    context.checkAdd(sym, stmt, newContext, pos)
    sym
  }

  def addDef(gens : List[Term], context : Context, stmt : Stmt, newContext : Context,
             name : String, pos : Position, isAbstract : Boolean, arity : Int) = {
    val sym = DefSymbol(name, isAbstract)
    sym.arity = arity
    sym.term = gens
    context.checkAdd(sym, stmt, newContext, pos)
    sym
  }

  def addDecl(context : Context, stmt : Stmt, newContext : Context,
              name : String, pos : Position, isMutable : Boolean) = {
    val sym = DeclSymbol(name, isMutable)
    context.checkAdd(sym, stmt, newContext, pos)
    sym
  }
}
