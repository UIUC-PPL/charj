package CharjParser

import scala.util.parsing.input.{Positional,Position}

object BaseContext {
  var context : Context = new Context(None, false)
}

class Context(parent : Option[Context], isOrdered : Boolean) {
  import scala.collection.mutable.ListBuffer
  var lst : ListBuffer[Symbol] = ListBuffer()
  var children : ListBuffer[Tuple2[Context, Stmt]] = ListBuffer()
  val ordered = isOrdered

  def checkAdd(sym : Symbol, pos : Position) {
    if (lst contains sym) {
      // @todo this check needs to be more specific
      val other = lst.find(_ == sym).get
      println("Semantic error: Conflict for " + sym + " at position " + pos + " and " + other.pos)
    } else {
      sym.setPos(pos)
      lst += sym
    }
  }

  def resolve(test : Symbol => Boolean) : Option[Symbol] = {
    val lookup = lst find test
    if (lookup.isEmpty) {
      parent match {
        case Some(x) => x.resolve(test)
        case None => None
      }
    } else lookup
  }

  override def toString = lst.toString
}
