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
              binding : List[(Term,Term)] = List()) : Option[Symbol] = {
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
            println("\t\t --- --- searching extensions: " + ext)
            //if (subtype == None) {
            //if (!isInst)
            //subtype = ext.cs.context.resolve(test, true, (sym.asInstanceOf[ClassSymbol].names, ext.generics))
            //else
            //subtype = ext.cs.context.resolve(test, true, Unifier.unifyTerms(sym.asInstanceOf[ClassSymbol].names, ext.cs.names, binding))
            //}
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

  // def resolve2(lst : ArrayBuffer[BoundClassSymbol], cur : Int, 
  //              test : ((Symbol, Context)) => Boolean) : Option[Symbol] = {
  //   if (lst.size > 0) {
  //     val res = lst.head.cs.context.resolve(test)
  //     if (!res.isEmpty) return res
  //     else return resolve2(lst.tail, cur + 1, test)
  //   } else return None
  // }

  // def useMapping(mapping : ListBuffer[(String, Symbol)], generics : ArrayBuffer[Symbol]) : ArrayBuffer[Symbol] = {
  //   generics.map(gen => {
  //     val gen2 = gen.asInstanceOf[BoundClassSymbol]
  //     var generics : ArrayBuffer[Symbol] = ArrayBuffer()
  //     var newBcs : BoundClassSymbol = null
  //     if (gen2.generics.size > 0)
  //       newBcs = BoundClassSymbol(gen2.cs, useMapping(mapping, gen2.generics))
  //     else {
  //       val found = Checker.tryFind(gen2.cs.name, mapping)
  //       val newCs = if (found.isEmpty) gen2.cs else found.get.asInstanceOf[ClassSymbol]
  //       newBcs = BoundClassSymbol(newCs, ArrayBuffer())
  //     }
  //     newBcs
  //   })
  // }

  // def instantiateWithTypeRecur(bcs : BoundClassSymbol) : ArrayBuffer[BoundClassSymbol] = {
  //   println("instantiateWithTypeRecur: for " + bcs)
  //   bcs.cs.subtypes.map(sub => {
  //     val mapping = bcs.cs.names zip bcs.generics
  //     BoundClassSymbol(sub.cs, useMapping(mapping, sub.generics))
  //   })
  // }

  def addInImplicits(parent : Context) {
    for ((sym,stmt,con) <- lst) {
      sym match {
        case ClassSymbol(name, _) => {
          val stmt2 = DefStmt(None,name,None,None,StmtList(List()))
          // artifically set position
          stmt2.pos = stmt.pos
          val artSym = DefSymbol(name, false)
          artSym.retType = Checker.resolveClassType(Type(BasicTypes.unitType), BaseContext.base)
          con.checkAdd(artSym, stmt2, null, stmt.pos)
        }
        case _ => ;
      }
      if (con != null) con.addInImplicits(this)
    }
  }

  override def toString = lst.unzip3._1.toString
}
