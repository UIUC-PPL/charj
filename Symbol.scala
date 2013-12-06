package CharjParser

import scala.util.parsing.input.{Positional,Position}

abstract class Symbol extends Positional {
  var isConstructor : Boolean = false
}

import scala.collection.mutable.ArrayBuffer
case class BoundClassSymbol(cs : ClassSymbol, bindings : List[(Term,Term)]) extends Symbol {
  override def toString = { cs.toString + ", bds = " + bindings }
}

case class ClassSymbol(name : String, arity : Int) extends Symbol {
  import scala.collection.mutable.ListBuffer
  var names : List[Term] = List()
  var isAbstract : Boolean = false
  var context : Context = new Context(None, false)
  override def toString = name + "[" + names + "]"
  var subtypes : ArrayBuffer[BoundClassSymbol] = ArrayBuffer()
}

case class DefSymbol(name : String, isAbstract : Boolean) extends Symbol {
  var inTypes : List[BoundClassSymbol] = List()
  var retType : BoundClassSymbol = null
  override def toString = "def \"" + name + "\", in = " + inTypes +
                          ", ret = " + retType + ", isConstructor = " + isConstructor
}

case class DeclSymbol(name : String, isMutable : Boolean) extends Symbol {
  var declType : BoundClassSymbol = null
  override def toString = (if (isMutable) "var" else "val") + " \"" + name + "\""
}

case class NoSymbol() extends Symbol
