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
              isInst : Boolean = false,
              binding : List[(Term,Term)] = List(),
              newTerm : Term = null) : Option[Symbol] = {
    println("resolve: sym = " + sym + ", binding = " + binding)
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
        val optParent = if (!parent.isEmpty) parent.get.resolve(test) else None

        // try parent classes to resolve symbol
        if (optParent.isEmpty) {
          var subtype : Option[Symbol] = None

          extensions.foreach(ext => {
            println("\t\t --- --- searching extensions: " + ext + ", nt = " + newTerm)
            if (subtype == None) {
              //if (!isInst)
                //subtype = ext.cs.context.resolve(test, true, (sym.asInstanceOf[ClassSymbol].names, ext.generics))
              //else
              val binds = ext.bindings ++ binding
              val nt = Unifier(true).subst(ext.cs.t, binds)
              subtype = ext.cs.context.resolve(test, true, ext.bindings ++ binding, nt)
            }
          })

          return subtype
        } else {
          return optParent
        }

      } else {
        return Some(cons.get._1)
      }
    } else {
      return Some(lookup.get._1)
    }
  }

  override def toString = lst.unzip3._1.toString
}
