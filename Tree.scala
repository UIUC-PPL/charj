package CharjParser

import scala.util.parsing.input.{Positional,Position}

trait GetName {
  def getName() = "stmt"
}

abstract class Stmt extends Positional with GetName {
  var context : Context = new Context(None, false)
}

case class ClassStmt(name : String, isSystem : Boolean, generic : Option[List[Type]],
                     parent : Option[Type], lst : List[Stmt]) extends Stmt {
  var sym : ClassSymbol = null
  override def getName() = pos + "-> class " + name
}
case class ChareStmt(name : String, lst : List[Stmt]) extends Stmt {
  override def getName() = pos + "-> chare " + name
}
case class DefStmt(isEntry : Option[String],
                   name : String,
                   nthunks : Option[List[TypeParam]],
                   ret : Option[Type],
                   stmts : List[Stmt]) extends Stmt {
  var sym : DefSymbol = null
  override def getName() = pos + "-> def " + name
}
case class DeclStmt(isMutable : Boolean, name : String, typ : Option[Type], expr : Expression) extends Stmt {
  var sym : DeclSymbol = null
  override def getName() = pos + "-> " + (if (isMutable) "var" else "val") + " " + name
}
case class StmtList(lst : List[Stmt]) extends Stmt {
  override def getName() = pos + "-> block"
}
case class ExprStmt(expr : Expression) extends Stmt
case class AssignStmt(lval : List[String], op : AssignOp, rval : Expression) extends Stmt
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

case class TypeParam(name : String, typ : Type) extends Stmt
case class Type(name : List[String], generic : Option[List[Type]]) extends Stmt

trait HasBoundClass { var sym : BoundClassSymbol = null }
abstract class Expression extends Positional with HasBoundClass

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
case class FunExpr(name : List[String], param : Option[List[Expression]]) extends Expression
case class NotExpr(el : Expression) extends Expression
case class NegExpr(el : Expression) extends Expression
case class StrExpr(name : List[String]) extends Expression
case class NewExpr(name : List[String],
                   generic : Option[List[Type]],
                   param : Option[List[Expression]]) extends Expression
case class True() extends Expression
case class False() extends Expression

abstract class AssignOp
case class Equal() extends AssignOp
case class PEqual() extends AssignOp
case class MEqual() extends AssignOp

