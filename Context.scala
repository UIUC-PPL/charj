package CharjParser

import scala.util.parsing.input.{Positional,Position}

object BaseContext {
  var context : Context = new Context(None, false)
  var base : Stmt = null
  var verbose : Boolean = false
}

class Context(parent : Option[Context], isOrdered : Boolean) {
  import scala.collection.mutable.ListBuffer
  var lst : ListBuffer[(Symbol, Stmt, Context)] = ListBuffer()
  var extensions : ListBuffer[Context] = ListBuffer()
  val ordered = isOrdered

  def checkAdd(sym : Symbol, stmt : Stmt, context : Context, pos : Position) {
    if (lst contains sym) {
      // @todo this check needs to be more specific
      val other = lst.find(_._1 == sym).get
      SemanticError("conflict for " + sym + " at position " + pos + " and " + other._1.pos, other._1.pos)
    } else {
      sym.setPos(pos)
      lst += Tuple3(sym, stmt, context)
    }
  }

  def resolve(test : Symbol => Boolean) : Option[Symbol] = {
    val lookup = lst.find(a => test(a._1))
    if (lookup.isEmpty) {
      val optParent =
        parent match {
          case Some(x) => x.resolve(test)
          case None => None
        }
      if (optParent.isEmpty) {
        val ret = resolve(extensions, test)
        ret
      }
      else optParent
    } else lookup match {
      case Some((a,_,_)) => Some(a)
      case None => None
    }
  }

  def resolve(lst : ListBuffer[Context], test : Symbol => Boolean) : Option[Symbol] = {
    if (lst.size > 0) {
      val res = lst.head.resolve(test)
      if (!res.isEmpty) return res
      else resolve(lst.tail, test)
    } else None
  }

  def addInImplicits(parent : Context) {
    for ((sym,stmt,con) <- lst) {
      sym match {
        case ClassSymbol(name, _) => {
          val stmt2 = DefStmt(None,name,None,None,List())
          // artifically set position
          stmt2.pos = stmt.pos
          val artSym = DefSymbol(name)
          artSym.retType = Checker.resolveClassType(BasicTypes.unitType, BaseContext.base)
          con.checkAdd(artSym, stmt2, null, stmt.pos)
        }
        case _ => ;
      }
      if (con != null) con.addInImplicits(this)
    }
  }

  override def toString = lst.unzip3._1.toString
}