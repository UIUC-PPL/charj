package edu.illinois.cs.charm

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{Stack}

class ScopeBuilder(tree : AstTraversable, verbose : Boolean) {
  class CurrentScope(var s : Stack[Scope]) { }

  def start() {
    val cur = new CurrentScope(new Stack[Scope]())
    tree.traverse(scopeStart, cur, scopeEnd)
    if (verbose)
      tree.traverse(printScope, cur, null)
  }

  def setScope(x : AstTraversable, con : CurrentScope) {
    x.escope = if (con.s.size > 0) Some(con.s.top) else None
  }

  def addParentToScope(s : Scope, con : CurrentScope) {
    // set current scope
    setScope(s, con);

    // add in parent scope to this new scope's list for resolution
    if (con.s.size > 0) {
      s.parentScopes += con.s.top
    }

    // push new scope on the stack
    con.s.push(s)
  }

  def printScope(x : AstTraversable, con : CurrentScope) = {
    println("x = " + x + ", scope = "
      + (if (!x.escope.isEmpty) x.escope.get else ""))
  }


  def scopeStart(x : AstTraversable, con : CurrentScope) : Unit = {
    x match {
      case m : EnumAst    => addParentToScope(m,con)
      case m : ClassAst   => addParentToScope(m,con)
      case m : DefAst     => addParentToScope(m,con)
      case m : Namespace  => addParentToScope(m,con)
      case _ => setScope(x, con);
    }
  }

  def scopeEnd(x : AstTraversable, con : CurrentScope) : Unit = {
    x match {
      case m:EnumAst   => con.s.pop
      case m:ClassAst  => con.s.pop
      case m:DefAst    => con.s.pop
      case m:Namespace => con.s.pop
      case _ => ;
    }
    x match {
      case m : ClassAst => setScope(m, con)
      case m : DefAst => {
        setScope(m.ident, con)
        setScope(m, con)
      }
      case _ => ;
    }
  }
}
