package CharjParser

import scala.util.parsing.input.{Positional,Position}

class Checker(tree : Stmt) {
  def start() = {

    println("--- traverse print classes ---")
    def filterClass(cls : Stmt) = cls.isInstanceOf[ClassStmt]
    def printClass(cls : Stmt) = println("found class name = " + cls.asInstanceOf[ClassStmt].name)
    new StmtVisitor(tree, filterClass, printClass);

    println("--- traverse resolve class type ---")
    new StmtVisitor(tree, filterClass, determineClassType);

    println("--- traverse resolve def type ---")
    def filterDefs(cls : Stmt) = cls.isInstanceOf[DefStmt]
    new StmtVisitor(tree, filterDefs, determineDefType)

    println("--- traverse resolve decl type ---")
    def filterDecls(cls : Stmt) = cls.isInstanceOf[DeclStmt]
    new StmtVisitor(tree, filterDecls, determineDeclType)

    println("--- traverse all ---")
    def noFilter(cls : Stmt) = true
    new StmtVisitor(tree, noFilter, traverseTree)
  }

  def determineDefType(tree : Stmt) {
    tree.asInstanceOf[DefStmt] match {
      case t@DefStmt(_, name, maybeParams, hasRet, lst) => {
        val lstParams = if (maybeParams.isEmpty) List() else maybeParams.get
        val inTypes = lstParams.map(tparam => resolveClassType(tparam.typ, tree))
        val retType = hasRet match {
          case Some(x) => resolveClassType(x, tree)
          case None => resolveClassType(Type(List("unit"), None), tree)
        }
        if (t.sym == null) {
          println("checker internal error")
          System.exit(5)
        }
        t.sym.inTypes = inTypes
        t.sym.retType = retType
        println(name + " def ret is " + retType + ", inTypes = " + inTypes)
      }
    }
  }

  def determineClassType(tree : Stmt) {
    tree.asInstanceOf[ClassStmt] match {
      case ClassStmt(name, _, _, Some(parent), _) => {
        val cls = resolveClassType(parent, tree)
        println(name + " resolved to subclass of " + cls)
      }
      case _ => ;
    }
  }

  def determineDeclType(tree : Stmt) {
    tree.asInstanceOf[DeclStmt] match {
      case t@DeclStmt(_, name, Some(typ), _) => {
        val sym = resolveClassType(typ, tree)
        val thisSym = tree.context.resolve(_ match {
          case DeclSymbol(n, _) => n == name
          case _ => false
        })
        if (sym == NoSymbol())
          println("Semantic error: " + name + " not resolved")
        else
          println("resolved " + name + " to " + thisSym.get + " with type: " + sym)
        thisSym.get.asInstanceOf[DeclSymbol].declType = sym.asInstanceOf[BoundClassSymbol]
      }
      case _ => ;
    }
  }

  def traverseTree(tree : Stmt) {
    tree match {
      case ExprStmt(expr) => {
        val typ = traverseExpr(tree, expr)
      }
      case _ => ;
    }

    def traverseExpr(cls : Stmt, expr : Expression) : Symbol = {
      expr match {
        case FunExpr(name, param) => {
          val exprs = if (param.isEmpty) List() else param.get
          val types = exprs.map(traverseExpr(cls, _))
          resolveFunType(cls, name, types)
        }
        case NumLiteral(str) => {
          val theInt = tryConvertToInt(str)
          if (!theInt.isEmpty) resolveClassType(Type(List("int"), None), tree)
          else NoSymbol()
        }
        case _ => NoSymbol()
      }
    }
  }

  def tryConvertToInt(s : String) : Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case _ : java.lang.NumberFormatException => None
    }
  }

  def resolveFunType(cls : Stmt, n : List[String], lst : List[Symbol]) : Symbol = {
    //@todo for now namespaces for types is not supported
    var context : Context = new Context(None)

    if (n.size == 2) {
      val decl = expectDeclSymbol(cls, n.head)
      if (!decl.isEmpty) {
        val typ = decl.get.declType
        println("resolved type for " + n.head + " = " + typ)
        context = typ.cs.context
      } else {
        println("Semantic error: could not resolve type for: " + n.head)
      }
    } else if (n.size == 1) {
      context = cls.context
    } else {
      println("Semantic error: namespaces not supported\n")
      System.exit(1)
    }

    val methodName = if (n.size == 1) n.head else n.tail.head

    val foundSym = context.resolve(_ match {
      case t@DefSymbol(name) => {
        if (name == methodName && t.inTypes.size == lst.size) {
          val toComp = (t.inTypes,lst).zipped.toList
          var isMatching = true
          for ((type1, type2) <- toComp) {
            if (type1 != type2) isMatching = false
          }
          isMatching
        } else false
      }
      case _ => false
    })

    if (foundSym.isEmpty) {
      println("Semantic error: def " + n + ", in = " + lst + ", unknown, searched context: " + context)
      NoSymbol()
      //System.exit(1)
    } else {
      println("resolved def to: " + foundSym.get)
      foundSym.get.asInstanceOf[DefSymbol].retType
    }
  }

  def expectDeclSymbol(cls : Stmt, str2 : String) : Option[DeclSymbol] = {
    val sym = cls.context.resolve(_ match {
      // make sure it was defined before
      case t@DeclSymbol(str, _) => str == str2 && t.pos < cls.pos
      case _ => false 
    })

    if (sym.isEmpty) {
      println("Semantic error: symbol " + str2 + " unknown")
      //System.exit(1)
      None
    } else {
      sym.get match {
        case t@DeclSymbol(_, _) => Some(t)
        case _ => None
      }
    }
  }

  def resolveClassType(t : Type, tree : Stmt) : BoundClassSymbol = {
    // @todo for now namespaces for types is not supported
    if (t.name.size != 1) {
      println("Semantic error: type " + t + " not supported")
      return null
    }

    val genTypes : List[Symbol] = t.generic match {
      case Some(typeList) => typeList.map(t => resolveClassType(t, tree))
      case None => List()
    }

    val className = t.name.head
    val optSym = tree.context.resolve(
      _ match {
        case ClassSymbol(name, num) => if (name != className || num != genTypes.size) false else true
        case _ => false
      }
    )

    if (optSym.isEmpty) {
      println("Semantic error: could not resolve type " + t.name.head + " at " + t.pos)
      //System.exit(1)
      null
    } else {
      optSym.get match {
        case t@ClassSymbol(_, _) => BoundClassSymbol(t, genTypes)
      }
    }
  }
}
