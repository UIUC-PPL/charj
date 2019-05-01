package edu.illinois.cs.charm

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{HashMap,ArrayStack}

object ModifyASTReplace {
  // Imprint all of the variable fields of src onto dst
  def imprint[T <: AstTraversable](dst : T, src : T) : T = {
    if (!(src eq dst)) {
      dst.ecls   = src.ecls
      dst.edef   = src.edef
      dst.escope = src.escope
      dst.eres   = src.eres
      dst.eprev  = src.eprev
      dst.etype  = src.etype
      dst.egen   = src.egen
      dst.eprox  = src.eprox
      dst.ered   = src.ered

      src match {
        case src : Assignable => {
          dst.asInstanceOf[Assignable].assigned = src.assigned
        }
        case src : LitType => {
          dst.asInstanceOf[LitType].string = src.string
        }
        case _ => ;
      }
    }

    dst
  }

  // Replace x with y in n
  def mkReplacement[T <: AstTraversable](n : T, x : AstTraversable, y : AstTraversable) : T = {
    imprint((n match {
      case _ if (n eq x)       => y
      case BinOp(lhs, op)      => new BinOp(lhs.map(n => mkReplacement(n, x, y)), op)
      case AssignAst(lhs, rhs) => new AssignAst(mkReplacement(lhs, x, y), mkReplacement(rhs, x, y))
      case LitType(lit, t)     => new LitType(lit, mkReplacement(t, x, y))
      case FuncCall(t, params) => new FuncCall(mkReplacement(t, x, y), params.map(n => mkReplacement(n, x, y)))
      case DeclAst(t,p,Some(xx)) if (xx eq x) => new DeclAst(t, p, Some(y))
      case ReturnAst(Some(expr)) => new ReturnAst(Some(mkReplacement(expr, x, y)))
      case t : Type            => t
    }).asInstanceOf[T], n)
  }

  def mkMSAReplacement(msa : String, innerRest : List[AstTraversable], outerRest : List[AstTraversable]) : AstTraversable = {
    innerRest match {
      case BinOp(lhs, op) :: _ => {
        // msa.access( lhs ) ==> pass op to outer
        var acc = new InternalGroup((new InternalNode(msa + "(")) :: lhs ::: (new InternalNode(")")) :: Nil)
        new BinOp(acc :: outerRest, op)
      }
    }
  }

  def findEnclosing(x : AstTraversable) : Option[AstTraversable] = {
    x.escope match {
      case Some(s) => {
        var rcon = new EnclContext(x)

        s.traverse(contextStart, rcon, contextEnd)

        if (rcon.ep.isDefined) {
          rcon.ep
        } else {
          Some(s)
        }
      }
      case None => None
    }
  }

  def replace(x : AstTraversable, y : AstTraversable) : Boolean = replace(x, y, findEnclosing(x))

  def replace(x : AstTraversable, y : AstTraversable, encl : Option[AstTraversable]) : Boolean = {
    encl match {
      case Some(encl) => replace(x, y, encl)
      case None       => false
    }
  }

  def replace(x : AstTraversable, y : AstTraversable, encl : AstTraversable) : Boolean = {
    x.escope match {
      case Some(s) => {
        if ((encl eq s) && (s.slst contains x)) {
          s.slst(s.slst.indexOf(x)) = y
          true
        } else if (s.slst contains encl) {
          s.slst(s.slst.indexOf(encl)) = mkReplacement(encl, x, y)
          true
        } else {
          println(s"Could not find $encl in $s, aborting replacement.")
          false
        }
      }
      case None => false
    }
  }

  private var already = false
  private def printTupleWarning(): Unit = {
    if (!already) println("WARNING : Tuples have not been implemented in Charj yet!")
    already = true
  }

  // TODO implement in the future
  def mkTupleType(types : List[Type]) : Type = {
    if (types.length != 1) printTupleWarning()
    types.head
  }

  def mkTupleGet(t : AstTraversable, i : Int) : AstTraversable = {
    if (i >= 1) printTupleWarning()
    t
  }
  def mkTuple(ts : List[AstTraversable]) : AstTraversable = {
    if (ts.length != 1) printTupleWarning()
    ts.head
  }

  class EnclContext(val x : AstTraversable) {
    val  s : ArrayStack[AstTraversable] = new ArrayStack[AstTraversable]
    var ep : Option[AstTraversable]     = None
  }

  def contextStart(x : AstTraversable, con : EnclContext) : Unit = {
    con.s.push(x)

    if (x eq con.x) {
      con.s.reverse.collectFirst {
        case s : Scope => s
      } match {
        case Some(s) => {
          con.ep = Some(con.s(con.s.indexOf(s) - 1))
        }
        case _ => ;
      }
    }
  }

  def contextEnd(x : AstTraversable, con : EnclContext) : Unit = {
    con.s.pop
  }
}

class ModifyASTInitial(tree : AstTraversable, verbose : Boolean) {
  def start() {
    // insert default constructor if no constructor is present
    tree.traverse(defaultCons, tree, null)
    // change super() calls to parent call for type checking and function matching
    tree.traverse(convertSuperCons, tree, null)
  }

  def defaultCons(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst if !m.istrait => {
        var foundCons = false

        for (stmt <- m.lst) {
          stmt match {
            case n : DefAst if n.cons => foundCons = true
            case _ => ;
          }
        }

        if (!foundCons) {
          if (verbose) {
            println("defaultCons: adding constructor to: " + m)
          }

          // if the class has a parent, insert a call to "super" with no arguments
          val superCall = if (m.parent.isEmpty) List() else List(Parse.createFunCallID("super", List()))

          val newCons = DefAst(List("constructor"), None,
            Type(Identifier(List("this")),List(), None),
            List(),
            Type(Identifier(List("unit")), List(), None),
            Some(superCall)
          )

          m.slst += newCons
        }
      }
      case _ => ;
    }
  }

  def convertSuperCons(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst if !m.parent.isEmpty && !m.istrait => {
        for (stmt <- m.slst) {
          stmt match {
            case n : DefAst if n.cons => {
              var foundSuperCall = false

              // search for super call
              for (semi <- n.slst) {
                semi match {
                  case o : FuncCall if o.t.name.lst.head == "super" => foundSuperCall = true
                  case _ => ;
                }
              }

              // add in super call if not found at the beginning of the constructor (not the end!)
              if (!foundSuperCall) n.slst.prepend(Parse.createFunCallID("super", List()))

              // convert super call to parent constructor call
              for (semi <- n.slst) {
                semi match {
                  case o : FuncCall if o.t.name.lst.head == "super" => {
                    if (verbose) {
                      println("convertSuperCons: " + m + ": found super call in constructor:" + o);
                    }

                    // set super to true, save old type to switch back later after type checking
                    o.superCall = true
                    o.oldSuperType = o.t

                    // copy parent constructor type
                    val clsType = m.parent.get
                    val clsGens = m.parent.get.lst.map(x => Type(x.name.copy(), x.lst, x.constraint))

                    // put in a call to the super class
                    o.t = clsType.copy(lst=clsGens)
                    o.t.setDeclare(false)
                    o.t.setOutside(false)
                  }
                  case _ => ;
                }
              }
            }
            case _ => ;
          }
        }
      }
      case _ => ;
    }
  }
}

class ModifyAsyncContext(var m : DefAst)

class ModifyASTPostEnclose(tree : AstTraversable, verbose : Boolean) {
  def start() {
    // change def constructor
    tree.traverse(defCons, tree, null)
    // change "this" keyword to enclosing class type
    tree.traverse(thisTypeConversion, tree, null)
    // change async def return types
    tree.traverse(defAsyncEnter, new ModifyAsyncContext(null), defAsyncLeave)
  }

  def defAsyncEnter(x : AstTraversable, con : ModifyAsyncContext) : Unit = {
    x match {
      case m : DefAst if m.async => {
        m.oldRet = m.ret
        m.ret = Type(Identifier(List("Future")),List(m.ret),None)
        m.ret.setDeclare(false)
        m.ret.setOutside(false)
        con.m = m
      }
      case m : ReturnAst if con.m != null => {
        val fc = BinOp(List(FuncCall(Type(Identifier(List("sendToFuture")),List(),None),
                       List(m.expr getOrElse BinOp(List(Identifier(List("unit"))),".")))),".")
        m.expr = Some(fc)
      }
      case _ => ;
    }
  }

  def defAsyncLeave(x : AstTraversable, con : ModifyAsyncContext) : Unit = {
    x match {
      case m : DefAst if m.async => con.m = null ;
      case _ => ;
    }
  }

  def defCons(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : DefAst if m.cons => {
        if (verbose) {
          println("defCons: found def constructor: " + m)
        }

        if (m.ecls.isEmpty) {
          SemanticError("def constructor must be inside a class", m.pos)
        } else {
          // change return type for constructor
          val clsType = m.ecls.get.ident
          val clsGens = m.ecls.get.ident.lst.map(x => Type(x.name, List(), None))
          m.oldRet = m.ret
          m.ret = clsType.copy(lst=clsGens)
          m.ret.setDeclare(false)
          m.ret.setOutside(false)
          if (verbose) {
            println("defCons: new return type for constructor: " + m.ret)
          }
        }
      }
      case _ => ;
    }
  }

  def thisTypeConversion(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      // this keyword
      case m : LitType if m.lit == "this" && m.x.name.lst.head == "any" => {
        if (m.ecls.isEmpty) {
          SemanticError("use of keyword \"this\" is not allowed outside of classes", m.pos)
        } else {
          val cls : ClassAst = m.ecls.get
          val clsGens = cls.ident.lst.map(x => Type(x.name, List(), None))
          m.x = cls.ident.copy(name=Identifier(List(cls.ident.name.lst.head)), lst=clsGens)
        }
      }
      case m : LitType if m.lit == "thisIndex" && m.x.name.lst.head == "any" => {
        if (m.ecls.isEmpty) {
          SemanticError("use of keyword \"thisIndex\" is not allowed outside of classes", m.pos)
        } else if (!m.ecls.get.chare) {
          SemanticError("use of keyword \"thisIndex\" is not allowed outside of chares", m.pos)
        } else if (m.ecls.get.charearray.isEmpty) {
          SemanticError("use of keyword \"thisIndex\" is not allowed outside of chare array types", m.pos)
        } else {
          val dim : Int = m.ecls.get.charearray.get.dim
          if (dim == 1) {
            m.x = Type(Identifier(List("int")), List(), None)
          } else if (dim == 2) {
            m.x = Type(Identifier(List("Index2")), List(), None)
          } else if (dim == 3) {
            m.x = Type(Identifier(List("Index3")), List(), None)
          }
          //println("m.x type = " + m.x)
        }
      }
      case m : LitType if m.lit == "thisProxy" && m.x.name.lst.head == "any" => {
        if (m.ecls.isEmpty) {
          SemanticError("use of keyword \"thisProxy\" is not allowed outside of classes", m.pos)
        } else if (!m.ecls.get.chare) {
          SemanticError("use of keyword \"thisProxy\" is not allowed outside of chares", m.pos)
        } else {
          val cls : ClassAst = m.ecls.get
          val clsGens = cls.ident.lst.map(x => Type(x.name, List(), None))
          val typeCopy = cls.ident.copy(name=Identifier(List(cls.ident.name.lst.head)), lst=clsGens)
          typeCopy.declare = false
          typeCopy.funcall = false
          val dim : Int = if (cls.charearray.isEmpty) 0 else cls.charearray.get.dim
          if (dim == 0) {
            m.x = Type(Identifier(List("Proxy")), List(typeCopy), None)
          } else if (dim == 1) {
            m.x = Type(Identifier(List("ProxyArray")), List(typeCopy), None)
          } else if (dim == 2) {
            m.x = Type(Identifier(List("ProxyArray2")), List(typeCopy), None)
          } else if (dim == 3) {
            m.x = Type(Identifier(List("ProxyArray3")), List(typeCopy), None)
          }
          //println("m.x type = " + m.x)
        }
      }
      case _ => ;
    }
  }
}
