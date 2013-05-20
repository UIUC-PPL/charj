package CharjParser

import scala.util.parsing.input.{Positional,Position}

class Collector(tree : Stmt) {
  def start() = traverseTree(tree, BaseContext.context)

  def print(context : Tuple2[Context, Stmt], indent : Int) {
    if (context._1.lst.size != 0) {
      val str = (0 to indent).map(_ => "\t").foldLeft("")((b,a) => b + a)
      println(str + context._2.getName() + ":  " + context._1.lst.toString())
    }
    for (child <- context._1.children) print(child, indent + 1)
  }

  def newContext(context : Context, stmt : Stmt) = {
    val con = new Context(Some(context))
    context.children += Tuple2(con, stmt)
    con
  }

  def traverseTree(tree : Stmt, context : Context) {
    tree.context = context
    tree match {
      case StmtList(lst) => traverseTree(lst, context)
      case t@ClassStmt(name, _, generic, _, lst) => {
        val arity = if (generic.isEmpty) 0 else generic.get.size
        val con = newContext(context, tree)
        if (!generic.isEmpty)
          for (gen <- generic.get)
            addClass(con, gen.name.head, 0, gen.pos)
        t.sym = addClass(context, name, arity, tree.pos)
        t.sym.context = con
        t.context = con
        traverseTree(lst, con)
      }
      case t@DefStmt(_, name, _, _, lst) => {
        t.sym = addDef(context, name, tree.pos)
        traverseTree(lst, newContext(context, tree))
      }
      case t@DeclStmt(mutable, name, _, expr) => {
        t.sym = addDecl(context, name, tree.pos, mutable)
      }
      case ForStmt(decls, _, cont, stmt) => {
        val con = newContext(context, tree)
        traverseTree(decls, con)
        traverseTree(stmt, con)
      }
      case IfStmt(_, stmt1, stmt2) => {
        traverseTree(stmt1, newContext(context, stmt1))
        if (!stmt2.isEmpty)
          traverseTree(stmt2, newContext(context, stmt2.get))
      }
      case WhileStmt(_, stmt) => traverseTree(stmt, newContext(context, stmt))
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

  def addClass(context : Context, name : String, arity : Int, pos : Position) = {
    val sym = ClassSymbol(name, arity)
    context.checkAdd(sym, pos)
    sym
  }

  def addDef(context : Context, name : String, pos : Position) = {
    val sym = DefSymbol(name)
    context.checkAdd(sym, pos)
    sym
  }

  def addDecl(context : Context, name : String, pos : Position, isMutable : Boolean) = {
    val sym = DeclSymbol(name, isMutable)
    context.checkAdd(sym, pos)
    sym
  }
}
