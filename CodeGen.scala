package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ArrayBuffer,ListBuffer}

class CodeGen(tree : Stmt, out : String => Unit) {
  import BaseContext.verbose

  def start() {
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt]
    new StmtVisitor(tree, filterClass, genClass);
  }

  def genClass(tree : Stmt) {
    tree match {
      case t@ClassStmt(name, isSystem, _, parent, _) if (!isSystem) => {
        out("class " + name + "{\n};")
      }
      case _ => ;
    }
  }

}
