package FrontEnd

import scala.util.parsing.input.{Positional,Position}

object DetermineEnclose {
  def setEnclosing[M <: Enclosing](x : M, c : Option[ClassAst], d : Option[DefAst]) : M = {
    x.ecls = c
    x.edef = d
    x
  }

  def setEnclosingRec[M <: AstTraversable](x : M, c : Option[ClassAst], d : Option[DefAst]) : M = {
    x.traverse((y : AstTraversable, con : Any) => { setEnclosing(y, c, d) }, null, null)
    x
  }
}

class DetermineEnclose(tree : AstTraversable, verbose : Boolean) {
  import DetermineEnclose.setEnclosing

  class EncloseContext(var c : Option[ClassAst], var d : Option[DefAst]) { }

  def start() {
    val context = new EncloseContext(None, None)
    tree.traverse(contextStart, context, contextEnd)
  }

  def contextStart(x : AstTraversable, con : EncloseContext) : Unit = {
    setEnclosing(x, con.c, con.d) match {
      case m : ClassAst => con.c = Some(m)
      case m : DefAst   => con.d = Some(m)
      case _ => {
        if (verbose) {
          println("x = " + x + ", class = " +
            (if (!x.ecls.isEmpty) x.ecls.get.ident.name else "")
            + ", def = " +
            (if (!x.edef.isEmpty) x.edef.get.ident.name else "")
          )
        }
      }
    }
  }

  def contextEnd(x : AstTraversable, con : EncloseContext) : Unit = {
    x match {
      case m : ClassAst => con.c = None
      case m : DefAst   => con.d = None
      case _ => ;
    }
  }
}
