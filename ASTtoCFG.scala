package CharjParser

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token._
import scala.util.parsing._
import scala.util.parsing.combinator.lexical._
import scala.io._
import scala.language.postfixOps
import scala.util.parsing.input.Positional
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

class Node {
  val incomingNodes: ListBuffer[Node] = ListBuffer()
  val outgoingNodes: ListBuffer[Node] = ListBuffer()
  var stmt: Stmt = null
  var expr: Expression = null
}

class Graph {
  var nodes: List[Node] = Nil
  def newNode: Node = {
    val res = new Node
    nodes = res :: nodes
    return res
  }
}

class ASTtoCFG(tree : Stmt) {
  import BaseContext.verbose
  var g: Graph = new Graph

  var enclosingClass : ClassStmt = null
  var defStack : Stack[DefStmt] = new Stack[DefStmt]()
  def getEnclosingDef() : DefStmt = if (defStack.isEmpty) null else defStack.top

  def start() : Node = {
    println("\033[0;31mstarting traverseTree\033[0m")
    var ntop = g.newNode
    traverseTree(tree, ntop)
    println("\033[0;31mtraverseTree done\033[0m")
    return ntop
  }

  def traverseTree(tree : Stmt, nprev : Node) : Node = {
    tree match {
      case StmtList(lst) => traverseTree(lst, nprev)

      case t@ClassStmt(_, _, _, parent, lst) => {
        // set enclosing
        enclosingClass = t
        tree.enclosingClass = t

        // TODO: what do we do in the conditional call to traverseTree?
        if (!parent.isEmpty) traverseTree(parent.get, nprev)
        var n = traverseTree(lst, nprev)

        // unset enclosing class
        enclosingClass = null

        return n
      }

      case t@DefStmt(_, gens, nth, ret, lst) => {
        println("\033[0;32mDeclStmt found in statement " + t + "\033[0m")
        // push enclosing def context
        defStack.push(t)
        t.enclosingDef = getEnclosingDef()
        tree.enclosingDef = getEnclosingDef()

        // if (!nth.isEmpty) {
        //   for (t <- nth.get) traverseTree(t)
        // }

        if (!ret.isEmpty) {
          ret.get.enclosingDef = getEnclosingDef()
          ret.get.enclosingClass = enclosingClass
        }

        var n = traverseTree(lst, nprev)

        // pop enclosing def context
        defStack.pop()

        return n
      }

      case t@DeclStmt(_, _, typ, mex) => {
        t.enclosingDef = getEnclosingDef()
        if (!typ.isEmpty) {
          typ.get.enclosingDef = getEnclosingDef()
        }

        var n = g.newNode
        n.incomingNodes += nprev
        nprev.outgoingNodes += n
        n.stmt = t

        return n
      }

      case t@IfStmt(cond, st1, st2) => {
        println("\033[0;32mIfStmt found in statement " + t + "\033[0m")
        var ncond = g.newNode
        nprev.outgoingNodes += ncond
        ncond.incomingNodes += nprev
        ncond.expr = cond

        var n1 = traverseTree(st1, ncond)
        var n2 : Node = null

        if (!st2.isEmpty) {
          n2 = traverseTree(st2.get, ncond)
        } else {
          n2 = g.newNode
          ncond.outgoingNodes += n2
          n2.incomingNodes += ncond
        }

        var join_node = g.newNode
        join_node.incomingNodes += n1
        join_node.incomingNodes += n2
        n1.outgoingNodes += join_node
        n2.outgoingNodes += join_node

        return join_node
      }

      case t@WhileStmt(cond, stmt) => {
        println("\033[0;32mWhileStmt found in statement " + t + "\033[0m")
        var ncond = g.newNode
        nprev.outgoingNodes += ncond
        ncond.incomingNodes += nprev
        ncond.expr = cond

        var nbody = traverseTree(stmt, ncond)

        var nexit = g.newNode
        ncond.outgoingNodes += nexit
        nexit.incomingNodes += ncond

        return nexit
      }

      case t@ForStmt(decls, expr1, cont, stmt) => {
        println("\033[0;32mForStmt found in statement " + t + "\033[0m")
        var ndecl = traverseTree(decls, nprev)

        var ncond = g.newNode
        ndecl.outgoingNodes += ncond
        ncond.incomingNodes += ndecl
        if (!expr1.isEmpty) {
          ncond.expr = expr1.get
        } else {
          ncond.expr = null
        }

        var ncont = traverseTree(cont, ncond)
        var nbody = traverseTree(stmt, ncont)

        var nexit = g.newNode
        ncond.outgoingNodes += nexit
        nexit.incomingNodes += ncond

        return nexit
      }

      case t@ExprStmt(expr1) => {
        println("\033[0;32mExprStmt found in statement " + t + "\033[0m")
        var nexpr = g.newNode
        nprev.outgoingNodes += nexpr
        nexpr.incomingNodes += nprev
        nexpr.expr = expr1

        return nexpr
      }

      case t@ReturnStmt(expr1) => {
        println("\033[0;32mReturnStmt found in statement " + t + "\033[0m")
        var nret = g.newNode
        nprev.outgoingNodes += nret
        nret.incomingNodes += nprev

        if (!expr1.isEmpty) {
          nret.expr = expr1.get
        }

        return nret
      }

      case t@AssignStmt(expr1,_,expr2) => {
        println("\033[0;32mAssignStmt found in statement " + t + "\033[0m")
        var n = g.newNode
        nprev.outgoingNodes += n
        n.incomingNodes += nprev
        n.stmt = t
        return n
      }

      case t@_ => {
        println("\033[0;33mfound something else in statement " + t + "\033[0m")
        return null
      }
    }
  }

  def traverseTree(lst : List[Stmt], nprev : Node) : Node = {
    var n : Node = null
    var ncurr = g.newNode
    nprev.outgoingNodes += ncurr
    ncurr.incomingNodes += nprev

    for (i <- lst) {
      n = traverseTree(i, ncurr)

      if (n != null) {
        ncurr.outgoingNodes += n
        n.incomingNodes += ncurr
        ncurr = n
      }
    }

    return ncurr
  }
}
