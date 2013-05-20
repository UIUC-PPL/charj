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
