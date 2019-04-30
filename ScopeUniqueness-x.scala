package FrontEnd

import scala.util.parsing.input.{Positional,Position}

class ScopeUniqueness(tree : AstTraversable, verbose : Boolean) {
  def start() {
    tree.traverse(uniqueStart, tree, null)
  }

  def checkUniqueness(x : AstTraversable, t : Type, s : Scope, params : Int) {
    if (verbose) {
      println("checkUniqueness: " + x)
    }

    for (at <- s.slst) {
      if (x.uid != at.uid) {
        if (verbose) {
          println("checkUniqueness: " + x + " with " + at)
        }
        at match {
          case m : ClassAst => {
            if (t.conflict(m.ident)) {
              SemanticErrorBin("symbol: " + t + " conflicts with " + m.ident, t.pos, m.ident.pos)
            }
          }
          case m : DefAst => {
            if (t.conflict(m.ident) && m.params.size == params) {
              SemanticErrorBin("symbol: " + t + " conflicts with " + m.ident, t.pos, m.ident.pos)
            }
          }
          case _ => ;
        }
      }
    }
  }

  def uniqueStart(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst => checkUniqueness(x, m.ident, x.escope.get, 0)
      case m : DefAst   => checkUniqueness(x, m.ident, x.escope.get, m.params.size)
      case _ => ;
    }
  }
}
