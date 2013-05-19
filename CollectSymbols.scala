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
      case ClassStmt(name, _, generic, _, lst) => {
        val arity = if (generic.isEmpty) 0 else generic.get.size
        val con = newContext(context, tree)
        if (!generic.isEmpty)
          for (gen <- generic.get)
            addClass(con, gen.name.head, 0, gen.pos)
        addClass(context, name, arity, tree.pos)
        traverseTree(lst, con)
      }
      case DefStmt(_, name, _, _, lst) => {
        addDef(context, name, tree.pos)
        traverseTree(lst, newContext(context, tree))
      }
      case DeclStmt(mutable, name, _, expr) => addDecl(context, name, tree.pos, mutable)
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

  def addClass(context : Context, name : String, arity : Int, pos : Position) =
    context.checkAdd(ClassSymbol(name, arity, List()), pos)

  def addDef(context : Context, name : String, pos : Position) =
    context.checkAdd(DefSymbol(name), pos)

  def addDecl(context : Context, name : String, pos : Position, isMutable : Boolean) =
    context.checkAdd(DeclSymbol(name, isMutable), pos)
}
