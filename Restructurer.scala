package CharjParser

import scala.util.parsing.input.{Positional,Position}

/*
 * This pass adding defs and decls to the tree based for
 * implicit constructors, etc.
 */

class Restructurer(t : Stmt) {
  import BasicTypes._
  import BaseContext.verbose
  
  def start() = traverseTree(t)

  def traverseTree(tree : Stmt) {
    tree match {
      case StmtList(lst) => lst.foreach(traverseTree(_))
      case t@ClassStmt(n, _, _, _, lst) => {
        val decls : List[(Int,DeclStmt)] = lst.map(traverseClassDecls(t,_))
        val numDecls = decls.unzip._1.foldLeft(0)(_+_)
        if (verbose) println("n = " + n + ", numDecls = " + numDecls)
        for (i <- 0 until (numDecls+1)) {
          val hasDef : Boolean = lst.map(traverseHasDefCount(n,i,_)).foldLeft(false)(_ || _)
          if (!hasDef) t.lst = generateDef(t, decls.unzip._2.take(i))::t.lst
          if (verbose) println("n = " + n + ", i = " + i + ", hasDef = " + hasDef)
        }
      }
      case _ => ;
    }
  }

  def generateDef(cs : ClassStmt, lst : List[DeclStmt]) : DefStmt = {
    val decls : List[(String,Option[Type])] = lst.map{a => Tuple2(a.name,a.typ)}
    val d : DefStmt = DefStmt(cs.name,
                              List(),
                              Some(decls.map{t => TypeParam(t._1 + "_", t._2.get)}),
                              None,
                              StmtList(
                                decls.map{t => AssignStmt(StrExpr(t._1), Equal(), StrExpr(t._1 + "_"))}
                              )
                            )
    d
  }

  def traverseHasDefCount(n : String, x : Int, tree : Stmt) = {
    tree match {
      case t@DefStmt(dname,_,nth,_,_) if (dname == n &&
                                          (!nth.isEmpty && nth.get.length == x) ||
                                          (nth.isEmpty && x == 0)) => true
      case _ => false
    }
  }

  def traverseClassDecls(cl : ClassStmt, tree : Stmt) = {
    tree match {
      case t@DeclStmt(_,_,typ,_) => {
        if (typ.isEmpty) SemanticError("declaration must have type", t.pos)
        Tuple2(1,t)
      }
      case _ => Tuple2(0,null)
    }
  }
}
