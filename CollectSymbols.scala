package CharjParser

import scala.util.parsing.input.{Positional,Position}

object BasicTypes {
  val booleanType = Type(List("boolean"), None)
  val intType = Type(List("int"), None)
  val unitType = Type(List("unit"), None)
  val refType = Type(List("Ref"), Some(List(Type(List("T"), None))))
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

  def traverseTree(tree : Stmt, context : Context) {
    tree.context = context
    tree match {
      case StmtList(lst) => traverseTree(lst, context)
      case t@ClassStmt(name, _, generic, _, lst) => {
        val arity = if (generic.isEmpty) 0 else generic.get.size
        val con = newContext(context, tree, false)
        t.sym = addClass(context, tree, con, name, arity, tree.pos)
        t.sym.context = con
        if (!generic.isEmpty)
          for (gen <- generic.get) {
            val tcon = newContext(con, DefStmt(None,name,None,Some(unitType), StmtList(List())), false)
            val sym = addClass(con, gen, tcon, gen.name.head, 0, gen.pos)
            sym.isAnything = true
            t.sym.names += gen.name.head
          }
        t.context = con
        traverseTree(lst, con)
      }
      case t@DefStmt(_, name, nthunks, _, lst) => {
        val con = newContext(context, tree, true)
        val isAbstract = lst == EmptyStmt()
        if (t.enclosingClass != null) {
          t.enclosingClass.sym.isAbstract = true
        }
        t.sym = addDef(context, tree, con, name, tree.pos, isAbstract)
        if (!nthunks.isEmpty) {
          for (param <- nthunks.get)
            addDecl(con, tree, null, param.name, param.pos, false)
        }
        traverseTree(lst, con)
      }
      case t@DeclStmt(mutable, name, _, expr) => {
        t.sym = addDecl(context, tree, null, name, tree.pos, mutable)
      }
      case ForStmt(decls, _, cont, stmt) => {
        val con = newContext(context, tree, true)
        traverseTree(decls, con)
        traverseTree(stmt, con)
      }
      case IfStmt(_, stmt1, stmt2) => {
        traverseTree(stmt1, newContext(context, stmt1, true))
        if (!stmt2.isEmpty)
          traverseTree(stmt2, newContext(context, stmt2.get, true))
      }
      case WhileStmt(_, stmt) => traverseTree(stmt, newContext(context, stmt, true))
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
