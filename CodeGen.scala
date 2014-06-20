package CharjParser

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ArrayBuffer,ListBuffer}

class CodeGen(tree : Stmt, out : String => Unit) {
  import BaseContext.verbose

  def start() {
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt]
    new StmtVisitor(tree, filterClass, genClassAll);

    def filterDefs(cls : Stmt) = cls.isInstanceOf[DefStmt]
    new StmtVisitor(tree, filterDefs, genDefs);
  }

  def outln(s : String) = out(s + "\n")

  def genClassName(n : String) = "__concrete_" + n
  def genDeclName(n : String, m : Boolean) = (if (m) "__var_" else "__val_") + n
  def genDefName(cl : String, n : String) = "__def_" + cl + "_" + n

  def genInner(tree : List[Stmt]) { for (t <- tree) genClassAll(t) }

  /*
   *
   * @ eric: I've started to sketch this out a bit more to make it
   * clearer what generating code will entail.
   * 
   */

  // generate the class wrapper and everything and belongs inside the
  // actual generated class structure
  def genClassAll(tree : Stmt) {
    tree match {
      case t@ClassStmt(name, isSystem, generic, parent, lst) => {
        // Ref will be handled specially
        if (!isSystem && !t.isAbstract && name != "Ref") {
          val genName = genClassName(name)

          outln("\n/* output class " + genName + "*/")

          outln("struct " + genName + " { /* parent is " + parent + "*/")
          genInner(lst)
          outln("};")

          outln("/* END output class " + genName + "*/")
        }
      }
      case t@DeclStmt(mutable,name,typ,expr) => {
        if (name != "this") {
          out(genType(typ, false) + " ")
          out(genDeclName(name, mutable))
          outln(";")
        }
      }
      case _ => ;
    }
  }

  // generate defs which will take the class as a parameter if there is one
  def genDefs(tree : Stmt) {
    tree match {
      case t@DefStmt(name,gens,nthunks,ret,stmts) => {
        var cl : String = ""
        if (t.enclosingClass != null)
          cl = genClassName(t.enclosingClass.name)
        val genName = genDefName(cl, name)

        // some special cases that we will handle later
        if (t.enclosingClass != null &&
            t.enclosingClass.name != "Ref" &&
            !t.enclosingClass.isAbstract &&
            !t.enclosingClass.isSystem) {

          outln("\n/* output function " + genName + "*/")

          if (!t.isEntry) {
            if (!t.isConstructor) {
              outln(genType(ret, false));
              outln(genName + "(");
              if (t.enclosingClass != null) {
                // first parameter is the class
                outln(genClassName(t.enclosingClass.name) + "* __OBJECT")
                if (!nthunks.isEmpty) out(",")
              }
              // generate input types
              out(genInputs(nthunks, false))
              outln(")");
              outln("{");
              // generate body of def
              outln("}");
            } else {
              // non-entry constructor
            }
          } else {
            // if it's an entry method generation is different
          }

          outln("/* END output function " + genName + "*/")
        }

      }
      case _ => ;
    }
  }

  def genInputs(ins : Option[List[TypeParam]], isRefType : Boolean) : String = {
    ins match {
      case Some(lst) => {
        if (lst.length != 0) {
          var str = genTypeParam(lst.head, isRefType)
          str += lst.takeRight(lst.length-1).map{ty => "," + genTypeParam(ty, isRefType)}.foldRight("")(_+_)
          str
        } else ""
      }
      case None => ""
    }
  }

  def genTypeParam(tp : TypeParam, isRefType : Boolean) : String =
    genType(Some(tp.typ), isRefType) + " " + genDeclName(tp.name, true) + "\n"

  def genType(typ : Option[Type], isRefType : Boolean) : String = {
    typ match {
      // this is totally wrong right now...
      case Some(Type(Bound(t))) => t
      case _ => "void"
    }
  }
}
