package CharjParser

import scala.util.parsing.input.{Positional,Position}

object BaseContext {
  var context : Context = new Context(None, false)
  var base : Stmt = null
  var verbose : Boolean = false
}

class Context(parent : Option[Context], isOrdered : Boolean) {
  import scala.collection.mutable.{ListBuffer,ArrayBuffer}
  var sym : Symbol = null
  var lst : ListBuffer[(Symbol, Stmt, Context)] = ListBuffer()
  var extensions : ArrayBuffer[BoundClassSymbol] = ArrayBuffer()
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

  def resolve(test : ((Symbol, Context)) => Boolean,
              binding : List[(Term,Term)] = List()) : Option[(Symbol,List[(Term,Term)])] = {
    //println("resolve: sym = " + sym + ", binding = " + binding)
    val lookup = lst.find(a => test((a._1, this)))
    if (lookup.isEmpty) {
      var cons : Option[(Symbol,Stmt,Context)] = None

      // strange constructor resolution?
      lst.foreach(a => a._1 match {
        case ClassSymbol(_,_) => {
          if (cons == None)
            cons = if (a._3 != null) a._3.lst.find{b => test((b._1, a._3)) && b._1.isConstructor} else None
        }
        case _ => ;
      })

      if (cons.isEmpty) {
        val optParent = if (!parent.isEmpty) parent.get.resolve(test, binding) else None

        // try parent classes to resolve symbol
        if (optParent.isEmpty) {
          var subtype : Option[(Symbol,List[(Term,Term)])] = None

          extensions.foreach(ext => {
            println("\t\t --- --- searching extensions: " + ext)
            if (subtype == None) {
              subtype = ext.cs.context.resolve(test, ext.bindings ++ binding)
            }
          })
          return subtype
        } else {
          return optParent
        }
      } else {
        return Some(cons.get._1, binding)
      }
    } else {
      return Some(lookup.get._1, binding)
    }
  }

  override def toString = lst.unzip3._1.toString
}
