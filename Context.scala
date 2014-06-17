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
  var extensions : ArrayBuffer[SingleType] = ArrayBuffer()
  val ordered = isOrdered

  def condSyms(sym1 : Symbol, sym2 : Symbol) : Boolean = {
    (sym1,sym2) match {
      case (t1@DefSymbol(n1,_),t2@DefSymbol(n2,_))
        if (n1 == n2 && (t1.arity != t2.arity || t1.name == "@afun" || t2.name == "@afun")) => false
      case (t1@DeclSymbol(n1,_),t2@DefSymbol(n2,_)) if (n1 == n2) => true
      case (t2@DefSymbol(n2,_),t1@DeclSymbol(n1,_)) if (n1 == n2) => true
      case (t1@DeclSymbol(n1,_),t2@DeclSymbol(n2,_)) if (n1 == n2) => true
      case (t1@DeclSymbol(n1,_),t2@ClassSymbol(n2,_)) if (n1 == n2) => true
      case (t1@ClassSymbol(n1,_),t2@DeclSymbol(n2,_)) if (n1 == n2) => true
      case (t1@ClassSymbol(n1,_),t2@ClassSymbol(n2,_)) if (n1 == n2) => true
      case (t1@ClassSymbol(n1,_),t2@DefSymbol(n2,_)) if (n1 == n2) => false
      case (t1@DefSymbol(n1,_),t2@ClassSymbol(n2,_)) if (n1 == n2) => false
      case (_,_) if (sym1.name == sym2.name) => true
      case (_,_) => false
    }
  }

  def checkAdd(sym : Symbol, stmt : Stmt, context : Context, pos : Position) {
    lst.find(a => condSyms(a._1,sym)) match {
      case Some(x) =>
        SemanticError("Symbol \"" + sym.name + "\" already defined at " + x._1.pos, pos)
      case None => {
        sym.setPos(pos)
        lst += Tuple3(sym, stmt, context)
      }
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
