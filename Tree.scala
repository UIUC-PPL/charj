package CharjParser

abstract class Stmt

case class ClassStmt(name : String, lst : List[Stmt]) extends Stmt
case class ChareStmt(name : String, lst : List[Stmt]) extends Stmt
case class DefStmt(isEntry : Option[String],
                   name : String,
                   nthunks : Option[List[TypeParam]],
                   ret : Option[Type],
                   stmts : List[Stmt]) extends Stmt
case class DeclStmt(isMutable : Boolean, name : String, typ : Type, expr : Expression) extends Stmt
case class StmtList(lst : List[Stmt]) extends Stmt
case class ExprStmt(expr : Expression) extends Stmt
case class AssignStmt(lval : List[String], rval : Expression) extends Stmt
case class IfStmt(cond : Expression, expr1 : Stmt, expr2 : Option[Stmt]) extends Stmt

case class TypeParam(name : String, typ : Type)
case class Type(name : List[String], generic : Option[List[Type]])

abstract class Expression

case class StrLiteral(text : String) extends Expression
case class NumLiteral(num : String) extends Expression

case class MulExpr(el : Expression, er : Expression) extends Expression
case class DivExpr(el : Expression, er : Expression) extends Expression
case class AddExpr(el : Expression, er : Expression) extends Expression
case class SubExpr(el : Expression, er : Expression) extends Expression
case class AndExpr(el : Expression, er : Expression) extends Expression
case class OrrExpr(el : Expression, er : Expression) extends Expression
case class ComExpr(el : Expression, er : Expression) extends Expression
case class FunExpr(name : List[String], param : Option[List[Expression]]) extends Expression
case class NotExpr(el : Expression) extends Expression
case class NegExpr(el : Expression) extends Expression
case class StrExpr(name : List[String]) extends Expression
case class NewExpr(name : List[String],
                   generic : Option[List[Type]],
                   param : Option[List[Expression]]) extends Expression
case class True() extends Expression
case class False() extends Expression
