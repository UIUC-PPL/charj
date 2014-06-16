package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ListBuffer,ArrayBuffer}

abstract class Symbol extends Positional {
  var isConstructor : Boolean = false
  val name : String
}

trait ResolvedType {
  var isNull : Boolean = false
  def getBindings() : List[(Term,Term)];
}
case class FunType(types : List[ResolvedType]) extends ResolvedType {
  var bindings : List[(Term,Term)] = List()
  def getBindings() : List[(Term,Term)] = types.map(_.getBindings()).reduceLeft[List[(Term,Term)]](_ ++ _) ++ bindings
}
case class SingleType(cs : ClassSymbol, val bindings : List[(Term,Term)]) extends Symbol with ResolvedType {
  def getBindings() : List[(Term,Term)] = bindings
  val name : String = ""
  override def toString = { cs.toString + ", bds = " + bindings }
}
// case class TermType(t : Term, val bindings : List[(Term,Term)]) extends Symbol with ResolvedType {
//   def getBindings() : List[(Term,Term)] = bindings
//   override def toString = { t + ", bds = " + bindings }
// }

case class ClassSymbol(name : String, arity : Int) extends Symbol {
  var t : Term = null
  var isAbstract : Boolean = false
  var context : Context = new Context(None, false)
  var parentBindings : ListBuffer[(Term,Term)] = ListBuffer()
  var stmt : ClassStmt = null
  var level : Int = -1
  var children : ListBuffer[ClassSymbol] = ListBuffer()
  override def toString = t.toString
}

trait HasDeclType {
  var declType : ResolvedType = null
}

case class DefSymbol(name : String, isAbstract : Boolean) extends Symbol with HasDeclType {
  var inTypes : List[ResolvedType] = List()
  var term : List[Term] = List()
  var retType : ResolvedType = null
  var isCons : Boolean = false
  var classCons : ClassStmt = null
  var arity : Int = -1
  override def toString = "def \"" + name + "\", in = " + inTypes +
                          ", ret = " + retType + ", isConstructor = " + isConstructor
}

case class DeclSymbol(name : String, isMutable : Boolean) extends Symbol with HasDeclType {
  override def toString = (if (isMutable) "var" else "val") + " \"" + name + "\""
}

case class NoSymbol() extends Symbol {
  val name : String = ""
}
