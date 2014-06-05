package CharjParser

import scala.util.parsing.input.{Positional,Position}

abstract class Symbol extends Positional {
  var isConstructor : Boolean = false
  val name : String
}

import scala.collection.mutable.ArrayBuffer
case class BoundClassSymbol(cs : ClassSymbol, val bindings : List[(Term,Term)]) extends Symbol {
  val name : String = ""
  var isNull : Boolean = false
  override def toString = { cs.toString + ", bds = " + bindings }
}

case class ClassSymbol(name : String, arity : Int) extends Symbol {
  import scala.collection.mutable.ListBuffer
  var t : Term = null
  var isAbstract : Boolean = false
  var context : Context = new Context(None, false)
  var level : Int = -1
  var children : ListBuffer[ClassSymbol] = ListBuffer()
  override def toString = t.toString
}

case class DefSymbol(name : String, isAbstract : Boolean) extends Symbol {
  var inTypes : List[BoundClassSymbol] = List()
  var retType : BoundClassSymbol = null
  var isCons : Boolean = false
  var classCons : ClassStmt = null
  var arity : Int = -1
  override def toString = "def \"" + name + "\", in = " + inTypes +
                          ", ret = " + retType + ", isConstructor = " + isConstructor
}

case class DeclSymbol(name : String, isMutable : Boolean) extends Symbol {
  var declType : BoundClassSymbol = null
  override def toString = (if (isMutable) "var" else "val") + " \"" + name + "\""
}

case class NoSymbol() extends Symbol {
  val name : String = ""
}
