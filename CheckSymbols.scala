package CharjParser

import scala.util.parsing.input.{Positional,Position}

class Checker(tree : Stmt) {
  def start() = traverseTree(tree)

  def traverseTree(tree : Stmt) {
    tree match {
      case StmtList(lst) => traverseTree(lst)
      case ClassStmt(_, _, _, Some(parent), lst) => {
        resolveClassType(parent, tree)
        traverseTree(lst)
      }
      case DeclStmt(_, name, Some(typ), _) => {
        val sym = resolveClassType(typ, tree)
        if (sym != NoSymbol())
          println(name +  " resolved to " + sym)
      }
      case DefStmt(_, _, Some(lstParams), Some(ret), lst) => {
        for (param <- lstParams)
          resolveClassType(param.typ, tree)
        resolveClassType(ret, tree)
        traverseTree(lst)
      }
      case ExprStmt(expr) => {
        val typ = traverseExpr(tree, expr)
      }
      case ClassStmt(_, _, _, _, lst) => traverseTree(lst)
      case DefStmt(_, _, _, _, lst)  => traverseTree(lst)
      case _ => ;
    }

    def traverseExpr(cls : Stmt, expr : Expression) : Symbol = {
      expr match {
        case FunExpr(name, param) => {
          val exprs = if (param.isEmpty) List() else param.get
          val types = exprs.map(traverseExpr(cls, _))
          resolveDefType(cls, name, types)
        }
        case _ => NoSymbol()
      }
    }
  }

  def traverseTree(lst : List[Stmt]) { for (stmt <- lst) traverseTree(stmt) }

  def resolveDefType(cls : Stmt, n : List[String], lst : List[Symbol]) : Symbol = {
    //@todo for now namespaces for types is not supported
    if (n.size == 1) {
      if (cls.context.resolve(_ == DefSymbol(n.head)).isEmpty) {
        println("Semantic error: def " + n + " unknown")
      }
    } else if (n.size == 2) {
      val sym = cls.context.resolve(_ match {
        // make sure it was defined before
        case t@DeclSymbol(str, _) => str == n.head && t.pos < cls.pos
        case _ => false 
      })
      if (sym.isEmpty)
        println("Semantic error: symbol " + n.head + " unknown")
    }

    NoSymbol()
  }

  def resolveClassType(t : Type, tree : Stmt) : Symbol = {
    // @todo for now namespaces for types is not supported
    if (t.name.size != 1) {
      println("Semantic error: type " + t + " not supported")
      return NoSymbol()
    }

    val genTypes : List[Symbol] = t.generic match {
      case Some(typeList) => typeList.map(t => resolveClassType(t, tree))
      case None => List()
    }

    val className = t.name.head
    val optSym = tree.context.resolve(
      _ match {
        case ClassSymbol(name, num, _) => if (name != className || num != genTypes.size) false else true
        case _ => false
      }
    )

    if (optSym.isEmpty) {
      println("Semantic error: could not resolve type " + t.name.head + " at " + t.pos)
      NoSymbol()
    } else {
      optSym.get match {
        case t@ClassSymbol(_, _, _) => t.copy(generics = genTypes)
      }
    }
  }
}
