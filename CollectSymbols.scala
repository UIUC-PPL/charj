package CharjParser

import scala.util.parsing.input.{Positional,Position}

object BasicTypes {
  val booleanType = Bound("boolean")
  val intType = Bound("int")
  val unitType = Bound("unit")
  val refType = Fun("Ref", List(MVar("T")))
}

class Collector(tree : Stmt) {
  import BasicTypes._

  def start() = {
    traverseTree(tree, BaseContext.context)
    BaseContext.context.addInImplicits(null)
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
      case t@ClassStmt(name, _, generic, _, lst) => {
        val arity = generic.size
        val con = newContext(context, tree, false)
        t.sym = addClass(context, tree, con, name, arity, tree.pos)
        t.sym.context = con
        con.sym = t.sym
        t.sym.names = generic
        // add generics to context for resolution, with a empty context
        for (gen <- generic) {
          val newCon = new Context(None, false)
          val sym = addClass(con, gen, newCon, gen.asInstanceOf[MVar].t, 0, gen.pos)
          t.sym.context.lst += ((sym, tree, newCon))
        }
        t.context = con
        enclosingClass = t
        traverseTree(lst, con)
        enclosingClass = null
      }
      case t@DefStmt(_, name, nthunks, _, lst) => {
        val con = newContext(context, tree, true)
        val isAbstract = lst == EmptyStmt()
        val isConstructor = t.enclosingClass != null && t.enclosingClass.name == name
        t.sym = addDef(context, tree, con, name, tree.pos, isAbstract)
        con.sym = enclosingClass.sym
        if (t.enclosingClass != null) {
          t.enclosingClass.sym.isAbstract = isAbstract
          t.enclosingClass.isAbstract = isAbstract
          t.sym.isConstructor = isConstructor
          t.isConstructor = isConstructor
        }
        if (!nthunks.isEmpty) {
          for (param <- nthunks.get)
            addDecl(con, tree, null, param.name, param.pos, false)
        }
        traverseTree(lst, con)
      }
      case t@DeclStmt(mutable, name, _, expr) => {
        if (expr.isEmpty && t.enclosingClass != null) {
          t.enclosingClass.sym.isAbstract = true
          t.enclosingClass.isAbstract = true
        }
        t.sym = addDecl(context, tree, null, name, tree.pos, mutable)
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
               name : String, arity : Int, pos : Position) = {
    val sym = ClassSymbol(name, arity)
    context.checkAdd(sym, stmt, newContext, pos)
    sym
  }

  def addDef(context : Context, stmt : Stmt, newContext : Context,
             name : String, pos : Position, isAbstract : Boolean) = {
    val sym = DefSymbol(name, isAbstract)
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
