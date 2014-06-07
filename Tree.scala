package CharjParser

import scala.util.parsing.input.{Positional,Position}

// If a new Stmt or Expression is added to this tree, remember to update the
// *Visitor classes

trait GetName {
  def getName() = "stmt"
}

abstract class Stmt extends Positional with GetName {
  var context : Context = new Context(None, false)
  var enclosingClass : ClassStmt = null
  var enclosingDef : DefStmt = null
}

case class ClassStmt(name : String, isSystem : Boolean, var generic : List[Term],
                     parent : Option[Type], var lst : List[Stmt]) extends Stmt {
  var sym : ClassSymbol = null
  var isAbstract : Boolean = false
  def getType() : Type = {
    if (generic == List()) Type(Bound(name))
    else Type(Fun(name, generic))
  }
  override def getName() = pos + "-> class " + name + "[isAbstract = " + isAbstract + "]"
}
case class ChareStmt(name : String, lst : List[Stmt]) extends Stmt {
  override def getName() = pos + "-> chare " + name
}
case class IncludeStmt(str : String) extends Stmt
case class DefStmt(isEntry : Option[String],
                   name : String,
                   var gens : List[Term],
                   nthunks : Option[List[TypeParam]],
                   ret : Option[Type],
                   stmts : Stmt) extends Stmt {
  var sym : DefSymbol = null
  var isConstructor : Boolean = false
  override def getName() = pos + "-> def " + name  + "[isConstructor = " + isConstructor + "]"
}
case class DeclStmt(isMutable : Boolean, name : String, typ : Option[Type], expr : Option[Expression]) extends Stmt {
  var sym : DeclSymbol = null
  override def getName() = pos + "-> " + (if (isMutable) "var" else "val") + " " + name
}
case class StmtList(lst : List[Stmt]) extends Stmt {
  override def getName() = pos + "-> block"
}
case class ExprStmt(expr : Expression) extends Stmt
case class AssignStmt(lval : Expression, op : AssignOp, rval : Expression) extends Stmt with HasBoundClass
case class IfStmt(cond : Expression, expr1 : Stmt, expr2 : Option[Stmt]) extends Stmt
case class ForStmt(decls : List[Stmt], expr1 : Expression, cont : List[Stmt], stmt : Stmt) extends Stmt {
  override def getName() = pos + "-> for"
}
case class WhileStmt(expr1 : Expression, stmt : Stmt) extends Stmt {
  override def getName() = pos + "-> while "
}
case class ReturnStmt(fact : Option[Expression]) extends Stmt {
  override def getName() = pos + "-> ret "
}
case class EmptyStmt() extends Stmt

case class TypeParam(name : String, typ : Type) extends Stmt with HasBoundClass {
  var decl : DeclSymbol = null
}
case class Type(var full : Term) extends Stmt {
  override def getName() = pos + "-> type " + full
}

trait HasBoundClass { var sym : BoundClassSymbol = null }
trait HasContext { var context : Context = null }
abstract class Expression extends Positional with HasBoundClass with HasContext

case class StrLiteral(text : String) extends Expression
case class NumLiteral(num : String) extends Expression

case class MulExpr(el : Expression, er : Expression) extends Expression // *
case class DivExpr(el : Expression, er : Expression) extends Expression // /
case class AddExpr(el : Expression, er : Expression) extends Expression // +
case class SubExpr(el : Expression, er : Expression) extends Expression // -
case class AndExpr(el : Expression, er : Expression) extends Expression // &&
case class OrrExpr(el : Expression, er : Expression) extends Expression // ||
case class ComExpr(el : Expression, er : Expression) extends Expression // ==
case class LesExpr(el : Expression, er : Expression) extends Expression // <
case class LeqExpr(el : Expression, er : Expression) extends Expression // <=
case class GesExpr(el : Expression, er : Expression) extends Expression // >
case class GeqExpr(el : Expression, er : Expression) extends Expression // >=
case class DotExpr(el : Expression, er : Expression) extends Expression // .
case class AopExpr(el : Expression, er : Expression, op : String) extends Expression // arbitrary operator
case class FunExpr(name : List[String], var generic : List[Term], param : Option[List[Expression]]) extends Expression
case class NotExpr(el : Expression) extends Expression
case class NegExpr(el : Expression) extends Expression
case class StrExpr(name : List[String]) extends Expression
case class NewExpr(name : List[String],
                   generic : List[Term],
                   param : Option[List[Expression]]) extends Expression
case class True() extends Expression
case class False() extends Expression
case class Null() extends Expression

abstract class AssignOp
case class Equal() extends AssignOp
case class PEqual() extends AssignOp
case class MEqual() extends AssignOp

