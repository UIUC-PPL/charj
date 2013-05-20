package CharjParser

class StmtVisitor[U >: Stmt](tree : Stmt, filter : U => Boolean, visit : U => Unit) {
  traverseTree(tree)

  def traverseTree(tree : Stmt) {
    tree match {
      case StmtList(lst) => traverseTree(lst)
      case t@ClassStmt(_, _, _, _, lst) => {
        maybeVisit(tree)
        traverseTree(lst)
      }
      case t@ChareStmt(_, lst) => {
        maybeVisit(tree)
        traverseTree(lst)
      }
      case t@DefStmt(_, _, _, _, lst) => {
        maybeVisit(tree)
        traverseTree(lst)
      }
      case t@DeclStmt(_, _, _, _) => maybeVisit(tree)
      case t@IfStmt(_, stmt1, stmt2) => {
        maybeVisit(tree)
        traverseTree(stmt1)
        if (!stmt2.isEmpty) traverseTree(stmt2.get)
      }
      case t@ForStmt(decls, _, cont, stmt) => {
        maybeVisit(tree)
        traverseTree(decls)
        traverseTree(cont)
        traverseTree(stmt)
      }
      case t@WhileStmt(_, stmt) => {
        maybeVisit(tree)
        traverseTree(stmt)
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
      case DeclStmt(_, _, _, expr) => visit(expr, tree)
      case ExprStmt(expr) => visit(expr, tree)
      case AssignStmt(_, _, expr) => visit(expr, tree)
      case IfStmt(expr, _, _) => visit(expr, tree)
      case ForStmt(_, expr, _, _) => visit(expr, tree)
      case WhileStmt(expr, _) => visit(expr, tree)
      case ReturnStmt(Some(expr)) => visit(expr, tree)
      case _ => ;
    }
  }

  def visitBinary(l : Expression, r : Expression, s : Stmt, cur : Expression) = {
    visit(l, s)
    visit(r, s)
    visit2(cur, s)
  }

  def visit(expr : Expression, s : Stmt) {
    expr match {
      case t@StrLiteral(_) => visit2(t, s)
      case t@NumLiteral(_) => visit2(t, s)
      case MulExpr(l, r) => visitBinary(l, r, s, expr)
      case DivExpr(l, r) => visitBinary(l, r, s, expr)
      case AddExpr(l, r) => visitBinary(l, r, s, expr)
      case SubExpr(l, r) => visitBinary(l, r, s, expr)
      case OrrExpr(l, r) => visitBinary(l, r, s, expr)
      case ComExpr(l, r) => visitBinary(l, r, s, expr)
      case LesExpr(l, r) => visitBinary(l, r, s, expr)
      case LeqExpr(l, r) => visitBinary(l, r, s, expr)
      case GesExpr(l, r) => visitBinary(l, r, s, expr)
      case GeqExpr(l, r) => visitBinary(l, r, s, expr)
      case t@FunExpr(_, params) => {
        if (!params.isEmpty) for (i <- params.get) visit(i, s)
        visit2(t, s)
      }
      case t@NotExpr(l) => { visit(l, s); visit2(t, s) }
      case t@NegExpr(l) => { visit(l, s); visit2(t, s) }
      case t@StrExpr(_) => visit2(t, s)
      case t@NewExpr(_, _, params) => {
        if (!params.isEmpty) for (i <- params.get) visit(i, s)
        visit2(t, s)
      }
      case t@True() => visit2(t, s)
      case t@False() => visit2(t, s)
      case _ => visit2(expr, s)
    }
  }
}
