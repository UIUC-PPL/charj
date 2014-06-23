package CharjParser

import scala.collection.mutable.Stack

class StmtVisitor[U >: Stmt](tree : Stmt, filter : U => Boolean, visit : U => Unit) {
  def visitExpressionForStmt(expr : Expression, t : Stmt) {
    new PureExprVisitor(expr, t, exprFunVisit)
  }

  def exprFunVisit(expr : Expression, tree : Stmt) {
    expr match {
      case DefExpr(dstmt) => traverseTree(dstmt)
      case _ => ;
    }
  }

  var enclosingClass : ClassStmt = null
  var defStack : Stack[DefStmt] = new Stack[DefStmt]()
  def getEnclosingDef() : DefStmt = if (defStack.isEmpty) null else defStack.top

  traverseTree(tree)

  def traverseTree(tree : Stmt) {
    tree.enclosingClass = enclosingClass
    tree.enclosingDef = getEnclosingDef()

    tree match {
      case StmtList(lst) => traverseTree(lst)
      case t@ClassStmt(_, _, _, parent, lst) => {
        // set enclosing
        enclosingClass = t
        tree.enclosingClass = t

        maybeVisit(tree)
        if (!parent.isEmpty) traverseTree(parent.get)
        traverseTree(lst)

        // unset enclosing class
        enclosingClass = null
      }
      case t@DefStmt(_, gens, nth, ret, lst) => {
        // push enclosing def context
        defStack.push(t)
        t.enclosingDef = getEnclosingDef()
        tree.enclosingDef = getEnclosingDef()

        maybeVisit(tree)

        if (!nth.isEmpty) {
          for (t <- nth.get) traverseTree(t)
        }

        if (!ret.isEmpty) {
          ret.get.enclosingDef = getEnclosingDef()
          ret.get.enclosingClass = enclosingClass
          maybeVisit(ret.get)
        }

        traverseTree(lst)

        // pop enclosing def context
        defStack.pop()
      }
      case t@TypeParam(_, typ) => {
        t.enclosingDef = getEnclosingDef()
        typ.enclosingDef = getEnclosingDef()
        maybeVisit(t); maybeVisit(typ)
      }
      case t@DeclStmt(_, _, typ, mex) => {
        t.enclosingDef = getEnclosingDef()
        if (!typ.isEmpty) {
          typ.get.enclosingDef = getEnclosingDef()
          maybeVisit(typ.get)
        }
        maybeVisit(tree)
        if (!mex.isEmpty) visitExpressionForStmt(mex.get, t);
      }
      case t@IfStmt(cond, stmt1, stmt2) => {
        maybeVisit(tree)
        traverseTree(stmt1)
        if (!stmt2.isEmpty) traverseTree(stmt2.get)
        visitExpressionForStmt(cond, t);
      }
      case t@ForStmt(decls, expr1, cont, stmt) => {
        maybeVisit(tree)
        traverseTree(decls)
        traverseTree(cont)
        traverseTree(stmt)
        if (!expr1.isEmpty) visitExpressionForStmt(expr1.get, t);
      }
      case t@WhileStmt(expr1, stmt) => {
        maybeVisit(tree)
        traverseTree(stmt)
        visitExpressionForStmt(expr1, t);
      }
      case t@ExprStmt(expr1) => {
        maybeVisit(tree)
        visitExpressionForStmt(expr1, t);
      }
      case t@ReturnStmt(expr1) => {
        maybeVisit(tree)
        if (!expr1.isEmpty) visitExpressionForStmt(expr1.get, t);
      }
      case t@EmptyStmt() => maybeVisit(tree)
      case t@IncludeStmt(_) => maybeVisit(tree)
      case t@AssignStmt(expr1,_,expr2) => {
        maybeVisit(tree)
        visitExpressionForStmt(expr1, t);
        visitExpressionForStmt(expr2, t);
      }
      case t@WaitStmt(funs,expr1,stmts) => {
        maybeVisit(tree)
        traverseTree(funs)
        traverseTree(stmts)
        if (!expr1.isEmpty) visitExpressionForStmt(expr1.get, t);
      }
      case _ => maybeVisit(tree)
    }
  }

  def maybeVisit(tree : Stmt) = if (filter(tree)) visit(tree)

  def traverseTree(lst : List[Stmt]) {
    for (i <- lst) traverseTree(i)
  }
}

class ExprVisitor[U >: Expression](tree : Stmt, visit2 : (U, Stmt) => Unit) {
  def noFilter(cls : Stmt) = true
  new StmtVisitor(tree, noFilter, traverseTree)

  def traverseTree(tree : Stmt) {
    tree match {
      case DeclStmt(_, _, _, Some(expr)) => visit(expr, tree)
      case ExprStmt(expr) => visit(expr, tree)
      case AssignStmt(expr1, _, expr2) => {visit(expr1, tree); visit(expr2, tree)}
      case IfStmt(expr, _, _) => visit(expr, tree)
      // hack to make sure that the stmt used to place the expr
      // (positionally) looks like it lexigraphically follows the
      // decls
      case ForStmt(_, Some(expr), _, stmt) => visit(expr, stmt)
      case WhileStmt(expr, _) => visit(expr, tree)
      case ReturnStmt(Some(expr)) => visit(expr, tree)
      case WaitStmt(_,Some(expr),_) => visit(expr, tree)
      case _ => ;
    }
  }

  def visit(expr : Expression, s : Stmt) {
    new PureExprVisitor(expr, s, visit2)
  }
}

class PureExprVisitor[U >: Expression](expr : Expression, s : Stmt, visit2 : (U, Stmt) => Unit) {
  visit(expr, s);

  def visitBinary(l : Expression, r : Expression, s : Stmt, cur : Expression) = {
    visit(l, s)
    visit(r, s)
    visit2(cur, s)
  }

  def visit(expr : Expression, s : Stmt) {
    expr match {
      case t@StrLiteral(_) => visit2(t, s)
      case t@NumLiteral(_) => visit2(t, s)
      case t@AsyncExpr(l) => {visit(l, s); visit2(t, s)}
      case t@SyncExpr(l) => {visit(l, s); visit2(t, s)}
      case t@DefExpr(stmt) => { visit2(t, s) }
      case MulExpr(l, r) => visitBinary(l, r, s, expr)
      case DivExpr(l, r) => visitBinary(l, r, s, expr)
      case ModExpr(l, r) => visitBinary(l, r, s, expr)
      case AddExpr(l, r) => visitBinary(l, r, s, expr)
      case SubExpr(l, r) => visitBinary(l, r, s, expr)
      case OrrExpr(l, r) => visitBinary(l, r, s, expr)
      case AndExpr(l, r) => visitBinary(l, r, s, expr)
      case ComExpr(l, r) => visitBinary(l, r, s, expr)
      case LesExpr(l, r) => visitBinary(l, r, s, expr)
      case LeqExpr(l, r) => visitBinary(l, r, s, expr)
      case GesExpr(l, r) => visitBinary(l, r, s, expr)
      case GeqExpr(l, r) => visitBinary(l, r, s, expr)
      case NeqExpr(l, r) => visitBinary(l, r, s, expr)
      case AopExpr(l, r, op) => visitBinary(l, r, s, expr)
      case DotExpr(l, r) => {
        // lhs traversal order, following by post, for resolution
        visit(l, s)
        visit2(expr, s)
        visit(r, s)
        visit2(expr, s)
      }
      case t@FunExpr(_, _, params) => {
        if (!params.isEmpty) for (i <- params.get) visit(i, s)
        visit2(t, s)
      }
      case t@NotExpr(l) => { visit(l, s); visit2(t, s) }
      case t@NegExpr(l) => { visit(l, s); visit2(t, s) }
      case t@StrExpr(_) => visit2(t, s)
      case t@NewExpr(l) => { visit(l, s); visit2(t, s) }
      case t@True() => visit2(t, s)
      case t@False() => visit2(t, s)
      case t@Null() => visit2(t, s)
      case _ => visit2(expr, s)
    }
  }
}
