package CharjParser

import scala.util.parsing.input.{Positional,Position}

abstract class Symbol extends Positional

case class BoundClassSymbol(cs : ClassSymbol, generics : List[Symbol]) extends Symbol {
  override def toString = {
    val genString = {
      if (generics.size > 0)
        "[" + generics.map(_.toString).foldLeft("")((b,a) => b + a) + "]"
      else ""
    }
    "class \"" + cs.name + "\" (" + cs.arity + ") " + genString
  }
}

case class ClassSymbol(name : String, arity : Int) extends Symbol {
  import scala.collection.mutable.ListBuffer
  var context : Context = new Context(None, false)
  override def toString = "class \"" + name + "\" (" + arity + ") "
  var subtypes : ListBuffer[BoundClassSymbol] = ListBuffer()
}

case class DefSymbol(name : String) extends Symbol {
  var inTypes : List[BoundClassSymbol] = List()
  var retType : BoundClassSymbol = null
  override def toString = "def \"" + name + "\", in = " + inTypes + ", ret = " + retType
}

case class DeclSymbol(name : String, isMutable : Boolean) extends Symbol {
  var declType : BoundClassSymbol = null
  override def toString = (if (isMutable) "var" else "val") + " \"" + name + "\""
}

case class NoSymbol() extends Symbol
