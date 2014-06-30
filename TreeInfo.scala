package CharjParser

class TreeInfo(tree : Stmt) {
  import BaseContext.verbose

  def start() { new StmtVisitor(tree, _.isInstanceOf[ClassStmt], info(0)); }

  def indent(i : Int) = (0 to i).map(_ => "\t").foldLeft("")(_+_)

  def info(i : Int)(cls : Stmt) {
    cls match {
      case t@ClassStmt(name,sys,_,_,lst) if (!sys) => {
        println(indent(i) + cls.pos + ": name = " + name + ", level = " + t.sym.level + ", abstract = " + t.isAbstract)
        new StmtVisitor(t, _.isInstanceOf[DefStmt], info(i+1))
      }
      case t@DefStmt(name,_,_,_,_) if (!t.isConstructor) => {
        var bpn = if (t.virtualBasePoint == null) "" else t.virtualBasePoint.enclosingClass.name
        println(indent(i) + cls.pos + ": name = " + name + ", virtual = " + t.isVirtual + ", basepoint = " + bpn)
      }
      case _ => ;
    }
  }
}
