package CharjParser

import scala.util.parsing.input.{Positional,Position}

object BaseContext {
  var context : Context = new Context(None, false)
}

class Context(parent : Option[Context], isOrdered : Boolean) {
  import scala.collection.mutable.ListBuffer
  var lst : ListBuffer[(Symbol, Stmt, Context)] = ListBuffer()
  val ordered = isOrdered

  def checkAdd(sym : Symbol, stmt : Stmt, context : Context, pos : Position) {
    if (lst contains sym) {
      // @todo this check needs to be more specific
      val other = lst.find(_._1 == sym).get
      println("Semantic error: Conflict for " + sym + " at position " + pos + " and " + other._1.pos)
    } else {
      sym.setPos(pos)
      lst += Tuple3(sym, stmt, context)
    }
  }

  def resolve(test : Symbol => Boolean) : Option[Symbol] = {
    val lookup = lst.find(a => test(a._1))
    if (lookup.isEmpty) {
      parent match {
        case Some(x) => x.resolve(test)
        case None => None
      }
    } else lookup match {
      case Some((a,_,_)) => Some(a)
      case None => None
    }
  }

  def addInImplicits(parent : Context) {
    for ((sym,stmt,con) <- lst) {
      sym match {
        case ClassSymbol(name, _) => {
          val stmt2 = DefStmt(None,name,None,None,List())
          // artifically set position
          stmt2.pos = stmt.pos
          con.checkAdd(DefSymbol(name), stmt2, null, stmt.pos)
        }
        case _ => ;
      }
      if (con != null) con.addInImplicits(this)
    }
  }

  override def toString = lst.toString
}
