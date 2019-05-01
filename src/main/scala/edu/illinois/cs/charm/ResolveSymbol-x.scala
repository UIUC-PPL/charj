package edu.illinois.cs.charm

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ListBuffer,HashMap}
import scala.util.control.Breaks._

object Symbol {
  import Parse.verbose

  def searchIdent(i : Identifier, s : ScopeIdent, fn : (ScopeIdent => Option[Resolvable]))
      : Option[(ScopeIdent,Resolvable)] = {
    val ex = ScopeIdent(s.scope.reverse)
    for (subscope <- ex.scope.tails) {
      val namespace = (i.lst.reverse ++ subscope).reverse
      if (verbose) {
        println("\t\t\t\t searching namespace: " + namespace)
      }
      val found = fn(ScopeIdent(namespace))
      if (!found.isEmpty) return Some(ScopeIdent(namespace),found.get)
    }
    return None
  }

  def permuteFst(s : ScopeIdent) : String = {
    val fst = s.scope.dropRight(1)
    val r = """[a-zA-Z_]+""".r
    val lst = r findFirstIn s.scope.last
    if (lst.isEmpty) ""
    else lst.get
  }

  def permuteInf(s : ScopeIdent) : ScopeIdent = ScopeIdent(s.scope.dropRight(1)) :+ permuteFst(s)

  def permuteRemainder(s : ScopeIdent) : String = {
    val fst = s.scope.dropRight(1)
    val r = """[0-9]+""".r
    val lst = (r findAllIn s.scope.last).toArray
    if (lst.size == 0) ""
    else lst.head
  }

  def getEnumInst(in : ScopeIdent) : Option[EnumInfInst] = {
    val res = ScopeExpansions.enumsx.get(permuteInf(in))
    if (res.isEmpty) None
    else {
      val rem = permuteRemainder(in)
      if (!res.get.lo.isEmpty) {
        if (rem.toInt < res.get.lo.get.toInt) return None
      }
      if (!res.get.hi.isEmpty) {
        if (rem.toInt > res.get.hi.get.toInt) return None
      }
      Some(EnumInfInst(res.get, permuteFst(in), permuteRemainder(in)))
    }
  }

  def resolvableToType(r : Resolvable) : Type =
    r match {
      case a : TypeParamAst  => a.param
      case a : DefAst        => a.ret
      case a : ClassAst      => a.ident
      case a : ClassGeneric  => a.t
      case a : DefGeneric    => a.t
      case a : EnumAst       => a.str
      case a : EnumInsideAst => a.str
      case a : EnumInfAst    => a.str
      case a : EnumInfInst   => {
        val t = a.e.str.copy(name=Identifier(List(a.e.str.name.lst.head + a.inst)))
        val gen = a.e.str.toGeneric
        gen match {
          case Bound(s,t1) => {
            t.generic = Some(Bound(ScopeIdent(s.scope.dropRight(1)) :+ (a.perm + a.inst), t))
            t.eres = Some((ScopeIdent(s.scope.dropRight(1) :+ (a.perm + a.inst)), a))
          }
          case _ => ;
        }
        t.einside = true
        t.escope = a.e.str.escope
        t
      }
    }

  val basicTypes : List[String] = List(
    "boolean",
    "int",
    "float",
    "double",
    "int16",
    "int64",
    "string",
    "unit",
    "any"
  )
  // basicRes => (eres, etype)
  val basicRes : HashMap[String,((ScopeIdent,Resolvable),(ScopeIdent, Generic, List[(Generic,Generic)]))] = HashMap()
}

class ResolveSymbol(tree : AstTraversable, verbose : Boolean) {
  import Symbol._

  class Collector(var unmarked : Boolean) {}
  class CollectorLevel(var unmarked : Boolean, var level : Int) {}

  def resolveBasic = {
    def searchfn(search : ScopeIdent) : Option[Resolvable] = ScopeExpansions.classes.get(search)

    for (b <- basicTypes) {
      val found = searchIdent(Identifier(List(b)), ScopeIdent(List(SystemIdentifier.namespace)), searchfn)

      if (found.isEmpty) {
        SemanticErrorNone("basic type: " + b + ", can not be found in system namespace");
      } else {
        basicRes.put(b, (found.get, resolvableToInst(found.get._2, List())))
      }
    }
  }

  def start() {
    if (verbose) println("ResolveSymbol: start resolve type name pass")
    tree.traverse(resolveTypeName, tree, null)

    if (verbose) println("ResolveSymbol: start generic resolution pass")
    tree.traverse(generic, tree, null)

    /// Resolve some basic types in base system namespace (no scope)
    resolveBasic
    ///

    if (verbose) println("ResolveSymbol: start traits check")
    tree.traverse(traitCheck, tree, null)

    if (verbose) println("ResolveSymbol: start unification pass")
    tree.traverse(unify, tree, null)

    // traverse tree of classes propagating information downward until
    // all classes are completed
    if (verbose) println("ResolveSymbol: start class abstract check pass")
    val c = new Collector(false)
    do {
      if (verbose) println("ResolveSymbol: class abstract collection: iterative")
      c.unmarked = false
      tree.traverse(collect, c, null)
    } while (c.unmarked);

    if (verbose) println("ResolveSymbol: start class collect virtual methods")
    val c2 = new CollectorLevel(false, 0)
    do {
      c2.unmarked = false
      tree.traverse(collectVirtual, c2, null)
      c2.level = c2.level + 1
      if (verbose) {
        println("ResolveSymbol: collectVirtual: level = " + c2.level + ", unmarked = " + c.unmarked)
      }
    } while (c2.unmarked);

    if (verbose) println("ResolveSymbol: start decl pass")
    tree.traverse(setDeclOrdered, tree, null)

    if (verbose) println("ResolveSymbol: start resolve identifier pass")
    tree.traverse(null, tree, resolveIdentifier)

    if (verbose) println("ResolveSymbol: start decl type pass")
    tree.traverse(null, tree, resolveDeclType)

    if (verbose) println("ResolveSymbol: start check return pass")
    tree.traverse(null, tree, checkReturnType)

    if (verbose) println("ResolveSymbol: start assignment type")
    tree.traverse(null, tree, checkAssignType)

    if (verbose) println("ResolveSymbol: start check set constructor pass")
    tree.traverse(null, tree, checkSetConstructor)

    if (verbose) println("ResolveSymbol: start check type for while statment condition")
    tree.traverse(null, tree, checkWhileCondition)

    if (verbose) println("ResolveSymbol: start check type for if statment condition")
    tree.traverse(null, tree, checkIfCondition)

    if (verbose) println("ResolveSymbol: start check type for for statment condition")
    tree.traverse(null, tree, checkForCondition)

    if (verbose) println("ResolveSymbol: start check type enum statement")
    tree.traverse(checkEnums, null, null)

    if (verbose) println("ResolveSymbol: start check decl initalizer pass")
    tree.traverse(checkDeclInitalizer, null, null)

    if (verbose) println("ResolveSymbol: check template classes do not extend")
    tree.traverse(checkTemplateClass, tree, null)

    if (verbose) println("ResolveSymbol: check abstract chares are fully abstract")
    tree.traverse(checkAbstractChare, tree, null)

    if (verbose) println("ResolveSymbol: check chares only extend abstract chares or trait chares")
    tree.traverse(checkExtendAbstractChare, tree, null)

    if (verbose) println("ResolveSymbol: check function return type on chares classes")
    tree.traverse(checkReturnChareDef, tree, null)
  }

  def addAbstractMethods(m : ClassAst) : Unit = {
    // add non-defined methods to interface list for base classes
    // create a defsymbol which just saves the generic type
    for (stmt <- m.lst) {
      stmt match {
        case n : DefAst if n.abs => m.interface += n.toDefMeta
        case _ => ;
      }
    }
  }

  def getMethods(m : ClassAst) : ListBuffer[DefAst] = {
    for (stmt <- m.slst; if stmt.isInstanceOf[DefAst]) yield
      stmt.asInstanceOf[DefAst]
  }

  // collect virtual methods in resolution order to provide manual override in generated code
  def collectVirtual(x : AstTraversable, col : CollectorLevel) : Unit = {
    x match {
      case m : ClassAst if m.level == col.level => {
        val parents = if (m.parent.isEmpty) m.traits else m.parent.get :: m.traits

        val myDefs = getMethods(m)
        for (x <- myDefs.filter(!_.abs).map(_.toDefMeta)) {
          // skip constructors
          if (x.name.lst.head != "this") {
            if (verbose) {
              println("collectVirtual (" + m.level + "): adding method: " + x + ", to: " + m)
            }
            m.thisMethods += x
          }
        }

        for (x <- myDefs.map(_.toDefMeta)) {
          if (x.name.lst.head != "this") m.thisMethodsAll += x
        }

        for (p <- parents) {
          //val defs : ListBuffer[DefAst] = getMethods(p.eres.get._2.asInstanceOf[ClassAst])
          val cls = p.eres.get._2.asInstanceOf[ClassAst]
          val resolution = p.resolution.get

          // excluding abstract methods for checking if virtual
          // override method needs to be generated
          val clsMeta = cls.thisMethods.map(_.instantiate(resolution._3))
          val inhMeta = cls.inheritMethods.map(_.instantiate(resolution._3))
          // add in inherited first
          for (x <- inhMeta ++ clsMeta) {
            x.ecls = Some(cls)
            m.inheritMethods += x
          }

          // including abstract methods for checking for Generic
          // hoisting when overloading value type
          val clsMetaAll = cls.thisMethodsAll.map(_.instantiate(resolution._3))
          val inhMetaAll = cls.inheritMethodsAll.map(_.instantiate(resolution._3))
          // add in inherited first
          for (x <- inhMetaAll ++ clsMetaAll) {
            x.ecls = Some(cls)
            m.inheritMethodsAll += x
          }
        }

        for (x <- m.inheritMethodsAll) {
          for (y <- m.thisMethodsAll) {
            if (verbose) {
              println("x = " + x)
              println("y = " + y)
              println("comparing x = " + x.name.systemNameDef(x.params.size) +  " and y = "
                + y.name.systemNameDef(x.params.size))
            }
            if (x.name.systemNameDef(x.params.size) == y.name.systemNameDef(y.params.size)) {
              if (!x.compare(y)) {
                SemanticErrorBin("name conflict between methods, types should the same", x.name.pos, y.name.pos)
              } else {
                y.edef.inherited = Some(x.edef)
              }
            }
          }
        }

        col.unmarked = true
      }
      case _ => ;
    }
  }

  def collect(x : AstTraversable, con : Collector) : Unit = {
    x match {
      case m : ClassAst => {
        if (!m.parent.isEmpty || m.traits.size != 0) {
          if (!m.parent.isEmpty && !m.parent.get.eres.get._2.isInstanceOf[ClassAst]) {
            SemanticError("parent must be a class, generics not allowed", m.pos);
          }

          val parentResolved = m.parent.isEmpty || m.parent.get.eres.get._2.asInstanceOf[ClassAst].level != -1
          val traitsResolved = m.traits.find(_.eres.get._2.asInstanceOf[ClassAst].level == -1).isEmpty

          val parentLevel = if (m.parent.isEmpty) 0 else m.parent.get.eres.get._2.asInstanceOf[ClassAst].level
          val maxTraitLevel = if (m.traits.size == 0) -1 else m.traits.map(_.eres.get._2.asInstanceOf[ClassAst].level).max
          val maxLevel = math.max(parentLevel, maxTraitLevel)
          if (parentResolved && traitsResolved) {
            m.level = maxLevel + 1

            addAbstractMethods(m)

            if (verbose) {
              println("collection (level " + m.level + "): accumulating: " + m)
            }

            val traitClasses = m.traits.map(_.eres.get._2.asInstanceOf[ClassAst])
            val traitResolutions = m.traits.map(_.resolution.get)

            val inherits =
              if (!m.parent.isEmpty)
                (m.parent.get.eres.get._2.asInstanceOf[ClassAst] :: traitClasses    ) zip
                (m.parent.get.resolution.get                     :: traitResolutions)
              else traitClasses zip traitResolutions

            for ((resolved,resolution) <- inherits) {
              if (resolved.abs) {
                for (n <- resolved.interface) {
                  val found = searchIdent(n.name.systemNameDef(n.params.size), m.ident.escope.get.expandName, searchfn_def(n.params.size))
                  val defMetaInst = n.instantiate(resolution._3)

                  if (verbose) {
                    println("interface bindings: " + resolution._3)
                    println("interface requirement: " + defMetaInst + ", input = " +
                      n.name + ", output = " + n.ret + ", found = "  + found + ", scopei = " +
                      m.escope.get.expandName)
                  }

                  if (found.isEmpty) {
                    m.interface += defMetaInst

                    if (verbose) {
                      println("for class (not found): " + m + ": adding interface requirement: " + defMetaInst)
                    }

                    if (!m.abs) {
                      SemanticErrorBin("class/trait must be marked abstract", m.pos, n.name.pos)
                    }
                  } else {
                    val curdef = found.get._2.asInstanceOf[DefAst]
                    val equal = defMetaInst.compare(curdef.toDefMeta)

                    if (equal && !curdef.lst.isEmpty) {
                      if (verbose) {
                        println("interface identical and defined " + defMetaInst)
                      }
                    } else {
                      m.interface += defMetaInst

                      if (verbose) {
                        println("for class: " + m + ": adding interface requirement: " + defMetaInst)
                      }

                      if (!m.abs) {
                        SemanticErrorBin("class/trait must be marked abstract", m.pos, n.name.pos)
                      }
                    }
                  }
                }
              }
            }
          } else {
            con.unmarked = true
          }
        } else {
          m.level = 0

          addAbstractMethods(m)

          if (verbose) {
            println("collection (level " + m.level + "): marking " + m + "as base level")
          }
        }
      }
      case m : DefAst => {
        if (verbose) {
          println("traversing " + m + " enclosing = " + m.ecls)
        }

        if (!m.ecls.isEmpty) {
          if (m.lst.isEmpty && !m.ecls.get.abs) {
            SemanticErrorBin("defs not defined (interface defs) must be inside a class marked abstract",
              m.pos, m.ecls.get.pos)
          }
        } else if (m.lst.isEmpty) {
          SemanticError("defs outside classes must be defined", m.pos)
        }
      }
      case _ => ;
    }
  }

  def resolveTypeName(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : Type if (!m.declare && !m.funcall) => {
        if (verbose) {
          println("resolveTypeName: trying to resolve: " + m + ", scope = " + m.escope)
          println("resolveTypeName: trying to resolve: " + m + ", expanded = " + m.escope.get.expandName)
        }

        // search for matching type name
        def searchfn(search : ScopeIdent) : Option[Resolvable] =
          ScopeExpansions.classes.get(search)    orElse
          ScopeExpansions.clsGeneric.get(search) orElse
          ScopeExpansions.defGeneric.get(search) orElse
          ScopeExpansions.enumsi.get(search)     orElse
          ScopeExpansions.enums.get(search)      orElse
          getEnumInst(search)

        val found = searchIdent(m.name.systemName, m.escope.get.expandName, searchfn)

        if (found.isEmpty) {
          SemanticError("type: " + x + " could not be found", x.pos)
        } else {
          if (verbose) {
            println("resolveTypeName: type = " + found);
            println("resolveTypeName: name = " + m.name);
          }
          found.get._2 match {
            case n : ClassAst => m.eres = Some((ScopeIdent(n.expandName.scope), found.get._2))
            case _            => m.eres = found
          }
        }
      }
      case _ => ;
    }
  }

  def generic(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : Type if !m.funcall => {
        m.generic = Some(m.toGeneric)
        if (verbose) {
          println("type generic: " + m.generic.get)
        }
      }
      case _ => ;
    }
  }

  def traitCheck(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst =>
        for (t <- m.traits) {
          val istrait = t.eres.get._2.asInstanceOf[ClassAst].istrait
          if (!istrait) {
            SemanticError("must specify a trait to extend", t.pos)
          }
        }
      case _ => ;
    }
    x match {
      case m : ClassAst if m.istrait => {
        for (stmt <- m.slst)
          stmt match {
            case n : DefAst if n.cons =>
              SemanticErrorBin("traits can not have constructors", m.pos, n.pos)
            case n : DeclAst =>
              SemanticErrorBin("traits can not have decls", m.pos, n.pos)
            case _ => ;
          }
      }
      case _ => ;
    }
  }

  def unify(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : Type if (!m.declare && !m.funcall) => {
        val classUse = m.generic.get
        val resolved = m.eres.get._2

        if (verbose) {
          println("unify pass: generic = " + classUse + ", resolution = " + resolved)
        }

        resolved match {
          case c : ClassAst => {
            val classDeclare = c.ident.generic.get
            if (verbose) {
              println("Try unify lhs = " + classUse)
              println("Try unify rhs = " + classDeclare)
            }
            val result = Unifier.unifyGeneric(classDeclare, classUse, List())
            if (verbose) {
              println("unification result = " + result)
            }
            m.resolution = Some(classUse, classDeclare, result)
          }
          case c : ClassGeneric => {
            m.resolution = Some((classUse, c.t.generic.get, List()))
          }
          case _ => ;
        }
      }
      case _ => ;
    }
  }

  def setDeclOrdered(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : TypeParamAst => m.ordered = !m.edef.isEmpty
      case _ => ;
    }
  }

  def resolvableToInst(r : Resolvable, in : List[(Generic,Generic)]) :
      (ScopeIdent, Generic, List[(Generic,Generic)]) = {
    val extract = resolvableToType(r)

    if (verbose) {
      println("\t\t in = " + in)
      println("\t\t extract = " + extract)
    }

    val classUse = extract.generic.get

    def searchfn(search : ScopeIdent) : Option[Resolvable] =
      ScopeExpansions.classes.get(search)    orElse
      ScopeExpansions.clsGeneric.get(search) orElse
      ScopeExpansions.defGeneric.get(search) orElse
      ScopeExpansions.enumsi.get(search)     orElse
      ScopeExpansions.enums.get(search)      orElse
      getEnumInst(search)

    if (verbose) {
      println("\t\t search for = " + classUse.t.name.systemName)
      println("\t\t in scope = " + classUse.t.escope.get.expandName)
    }

    val foundDeclare = searchIdent(classUse.t.name.systemName, classUse.t.escope.get.expandName, searchfn)

    if (verbose) {
      println("\t\t classUse = " + classUse)
      println("\t\t foundDeclare = " + foundDeclare)
    }

    val classDeclare = resolvableToType(foundDeclare.get._2).toGeneric
    val classUseInst = Unifier.subst(classUse, in)

    if (verbose) {
      println("\t\t classUse = " + classUse)
      println("\t\t foundDeclare = " + foundDeclare)
      println("\t\t classDeclare = " + classDeclare)
      println("\t\t classUseInst = " + classUseInst)
    }

    val bindings = Unifier.unifyGeneric(classDeclare, classUseInst, List())
    val classUseNext = Unifier.subst(classDeclare, bindings)

    if (verbose) {
      println("\t\t classUse = " + classUse)
      println("\t\t foundDeclare = " + foundDeclare)
      println("\t\t classDeclare = " + classDeclare)
      println("\t\t classUseInst = " + classUseInst)
      println("\t\t bindings = " + bindings)
      println("\t\t scope (decl) to search = " + classDeclare.t.name)
      println("\t\t scope (inst) to search = " + classUseInst.t.name)
      println("\t\t subst = " + classUseNext)
    }

    val found = searchIdent(classUseInst.t.name.systemName, classUseInst.t.escope.get.expandName, searchfn)
    val cls = resolvableToType(found.get._2)
    val clsDeclare = cls.toGeneric
    val bindings2 = Unifier.unifyGeneric(clsDeclare, classUseNext, List())

    if (verbose) {
      println("\t\t decl = " + found)
      println("\t\t classUseNext = " + classUseNext)
      println("\t\t clsDeclare = " + clsDeclare)
      println("\t\t bindings2 = " + bindings2)
    }

    if ((classUseNext.isInstanceOf[FreeVar] || classUseNext.isInstanceOf[FreeVarConstraint])
        && !cls.constraint.isEmpty) {
      val cons = cls.constraint.get
      val bindings2 = cons.resolution.get._3
      val use = cons.resolution.get._2
      val scope = cons.eres.get._1

      if (verbose) {
        println("\t\t freevar found = " + cls)
        println("\t\t freevar found with resolution = " + cons.resolution)
      }

      (scope, use, bindings2)
    } else {
      (cls.escope.get.expandName, classUseNext, bindings2)
    }
  }

  // check if a function declaration types line up with the function call
  def checkFuncParams(d : DefAst, m : FuncCall, binds : List[(Generic,Generic)]) : List[(Generic,Generic)] = {
    var declGen = d.ident.toGeneric
    var useGen = m.t.toGeneric

    // if the declared function is a constructor switch with enclosing class type
    if (d.cons) {
      declGen = d.ecls.get.ident.toGeneric
      // remove "this" from end of use type
      useGen = useGen match {
        case n : Container => Container(ScopeIdent(n.str.scope.dropRight(1)), n.lst, n.t1)
        case n : Bound     => Bound(ScopeIdent(n.str.scope.dropRight(1)), n.t1)
      }
    }

    // automatically try to instantiate based on arguments types in
    // that order if a generic is not specified at the call site but
    // is present at the declaration
    (declGen,useGen) match {
      case (Container(_,_,_),d@Bound(_,_)) => {
        val fcall = m.params.map(_.etype.get._2)
        useGen = Container(d.str, fcall, d.t1)
      }
      case _ => ;
    }

    if (verbose) {
      println("\t checkFuncParams: declGen = " + declGen)
      println("\t checkFuncParams: useGen = " + useGen)
    }

    val result = Unifier.unifyGeneric(declGen, useGen, List())

    if (verbose) {
      println("\t checkFuncParams: unify result = " + result)
    }

    val fdecl = d.params.map(_.param.toGeneric)
    val fcall = m.params.map(_.etype.get._2)
    val zipped = fdecl zip fcall
    var i = 0
    for (z <- zipped) {
      if (verbose) {
        println("\t function param declare = " + z._1)
        println("\t function param use = " + z._2)
        println("\t function param binds = " + binds)
      }
      val fdeclrepl = Unifier.subst(z._1, binds ++ result)

      if (verbose) {
        println("\t function check equality: term1 = " + fdeclrepl + ", term2 = " + z._2)
      }
      // z = (declare, use), order matters here!
      val equal = Unifier.isEqual(fdeclrepl, z._2)
      if (!equal) {
        SemanticErrorBin("function parameter type mismatch", z._1.t.pos, m.params(i).pos)
      }
      i = i + 1
    }

    return binds ++ result
  }

  def resolveSuperclass(m : AstTraversable, id : Identifier, search_sym : ScopeIdent => Option[Resolvable],
                        scope : ScopeIdent, cls : Generic, binds : List[(Generic,Generic)])
      : Option[(Option[(ScopeIdent,Resolvable)], List[(Generic,Generic)])] = {
    def searchfn(namespace : ScopeIdent) : Option[Resolvable] = ScopeExpansions.classes.get(namespace)
    val found = searchIdent(cls.t.name, scope, searchfn)

    if (found.isEmpty) return None

    if (verbose) {
      println("resolveSuperclass: found class = " + found)
    }

    var curfound : Option[(Option[(ScopeIdent,Resolvable)], List[(Generic,Generic)])] = None
    val curcls = found.get._2.asInstanceOf[ClassAst]

    if (!curcls.parent.isEmpty) {
      val parentType = curcls.parent.get
      val parent = parentType.eres.get._2.asInstanceOf[ClassAst]
      val par_scope = parentType.eres.get._1
      val resolution = parentType.resolution.get

      if (verbose) {
        println("resolveSuperclass: found parent class = " + parent)
        println("resolveSuperclass: parent resolution = " + resolution)
      }

      val found = searchIdent(id, parentType.eres.get._1, search_sym)

      if (!found.isEmpty)
        return Some(found, resolution._3)
      else
        curfound = resolveSuperclass(m, id, search_sym, par_scope, resolution._1, binds ++ resolution._3)
    }

    if (!curfound.isEmpty) {
      return curfound
    } else {
      for (t <- curcls.traits) {
        val traitCls = t.eres.get._2.asInstanceOf[ClassAst]
        val trait_scope = t.eres.get._1
        val resolution = t.resolution.get
        val found = searchIdent(id, t.eres.get._1, search_sym)

        if (!found.isEmpty)
          return Some(found, resolution._3)
        else
          curfound = resolveSuperclass(m, id, search_sym, trait_scope, resolution._1, binds ++ resolution._3)

        if (!curfound.isEmpty) return curfound
      }
    }

    return None
  }

  def searchfn_def(len : Int)(namespace : ScopeIdent) : Option[Resolvable] = {
    val result = ScopeExpansions.defs.get(namespace)
    // found the right symbol and the number of parameters match
    if (!result.isEmpty && result.get.params.size == len) result
    else None
  }

  def resolveFindConstructor(m : FuncCall, scope : ScopeIdent) : Option[(ScopeIdent,Resolvable)] =  {
    if (verbose) println("resolveFindConstructor: trying to find: " + m)

    def searchfn(name : ScopeIdent) : Option[Resolvable] = ScopeExpansions.classes.get(name)
    val name = m.t.name.systemName
    val found = searchIdent(name, scope, searchfn)

    if (verbose) println("resolveFindConstructor: found class: " + found)

    if (!found.isEmpty) {
      found.get._2 match {
        case n : ClassAst => {
          val scope = n.escope.get.expandName :+ n.ident.name.lst.head
          val found2 = searchIdent(Identifier(List("this")).systemNameDef(m.params.size), scope, searchfn_def(m.params.size))
          if (verbose) println("resolveFindConstructor: found constructor with matching params: " + found2)
          return found2
        }
        case _ => return None
      }
    } else {
      return None
    }
  }

  def depromoteProxyType(x : AstTraversable) {
    x.etype.get._2 match {
      case m : Container if ProxyType.isProxyType(m.str.scope.last) => {
        if (verbose) {
          println("found proxy: " + m + ", res = " + x.eres + ", etype = " + x.etype)
        }
        val newident = Identifier(List("get"))
        newident.eprev = Some(x)
        resolveIdentifier(newident, null)
        if (verbose) {
          println("found proxy type: " + newident.eres + ", etype = " + newident.etype)
        }
        if (x.eres.isEmpty) {
          x.eres = newident.eres
        }
        x.etype = newident.etype
        x.eprox = Some(ProxyType.to_eprox(m.str.scope.last))
      }
      case _ => ;
    }
  }

  def promoteProxyType(x : AstTraversable, l : Int) {
    val newfun = l match {
      case 0 => FuncCall(Type(Identifier(List("make_proxy")),        List(), None), List(x))
      case 1 => FuncCall(Type(Identifier(List("make_proxy_array")),  List(), None), List(x))
      case 2 => FuncCall(Type(Identifier(List("make_proxy_array2")), List(), None), List(x))
      case 3 => FuncCall(Type(Identifier(List("make_proxy_array3")), List(), None), List(x))
    }
    newfun.escope = x.escope
    resolveIdentifier(newfun, null)
    if (verbose) {
      println("promoted proxy type: " + newfun.eres + ", etype = " + newfun.etype)
    }
    x.eres = newfun.eres
    x.etype = newfun.etype
  }

  def resolveIdentifier(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : FuncCall if (m.t.name.lst.head == "==" || m.t.name.lst.head == "!=")
                            && !m.eprev.isEmpty && m.params.size == 1 => {
        val tprev = m.eprev.get.etype.get._2
        val input = m.params.head.etype.get._2
        if (verbose) println("funcall specialized (==): " + m + ", prev = " + tprev + ", input = " + input)
        val isany = ScopeIdent(List(SystemIdentifier.namespace, "any"))
        if (!(input.si == isany || tprev.si == isany)) {
          val equal = Unifier.isEqual(tprev,input)
          if (!equal) {
            SemanticErrorBin("must be comparable types: ", tprev.t.pos, input.t.pos)
          }
        }

        m.eres = Some(basicRes("boolean")._1)
        m.etype = Some(basicRes("boolean")._2)
      }

      case m : FuncCall => {
        if (verbose) {
          println("funcall: " + m + ", prev = " + m.eprev)
          if (!m.eprev.isEmpty) {
            println("prev type: " + m.eprev.get.etype)
          }
        }

        val prev = m.eprev

        // depromote proxy types to resolve type correctly
        if (!m.eprev.isEmpty &&
            (m.t.name.lst.head != "[]" &&
             m.t.name.lst.head != "contribute" &&
             m.t.name.lst.head != "destroy")) {
          depromoteProxyType(m.eprev.get)
        }

        if (!prev.isEmpty && prev.get.etype.isEmpty) {
          SemanticError("could not resolve symbol because previous not found: " + m, m.pos)
        }

        // if no previous context to search, start with identifier's namespace
        val scope = if (prev.isEmpty) m.escope.get.expandName else prev.get.etype.get._1
        var binds = if (prev.isEmpty) List()                  else prev.get.etype.get._3

        val name = m.t.name.systemNameDef(m.params.size)
        var found = searchIdent(name, scope, searchfn_def(m.params.size))

        if (verbose) {
          println("funcall: searching for funcall:" + name)
        }

        if (found.isEmpty) {
          // skip super class if there is not a previous or an enclosing class
          if (!prev.isEmpty || !m.ecls.isEmpty) {
            val gen = if (prev.isEmpty) m.ecls.get.ident.toGeneric else prev.get.etype.get._2
            val superResult = resolveSuperclass(m, name, searchfn_def(m.params.size), scope, gen, binds)

            if (!superResult.isEmpty) {
              found = superResult.get._1
              binds = binds ++ superResult.get._2
            }
          }
        }

        // if we still have not found it try searching for a constructor
        if (found.isEmpty) {
          found = resolveFindConstructor(m, scope)
        }

        // now search for an enum extension type
        if (found.isEmpty && !m.eprev.isEmpty) {
          val t = m.eprev.get.eres.get._2 match {
            case m : TypeParamAst  => {
              m.param.eres.get._2 match {
                case m : EnumAst       => Some(m.param)
                case m : EnumInsideAst => Some(m.enum.get.param)
                case m : EnumInfAst    => Some(m.enum.get.param)
                case _                 => None
              }
            }
            case _                 => None
          }

          if (!t.isEmpty) {
            val scope = t.get.eres.get._1
            binds = t.get.resolution.get._3

            found = searchIdent(name, scope, searchfn_def(m.params.size))

            if (found.isEmpty) {
              val superResult = resolveSuperclass(m, name, searchfn_def(m.params.size), scope,
                                                  t.get.resolution.get._2, binds)

              if (!superResult.isEmpty) {
                found = superResult.get._1
                binds = binds ++ superResult.get._2
              }
            }
          }
        }

        if (verbose) {
          println("funcall: " + m + ", found = " + found)
        }

        if (!found.isEmpty) {

          // set resolution for the type associated with the function call
          found.get._2 match {
            case n : DefAst => m.t.eres = Some((ScopeIdent(n.expandName.scope), found.get._2))
            case _          => m.t.eres = found
          }

          // check that the function params match input types
          binds = checkFuncParams(found.get._2.asInstanceOf[DefAst], m, binds)

          m.eres = found
          m.etype = Some(resolvableToInst(found.get._2, binds))
        } else {
          if (m.superCall) {
            SemanticError("could not find matching parent constructor: " + m.oldSuperType, m.pos)
          } else {
            SemanticError("could not find symbol: " + m, m.pos)
          }
        }
      }

      case m : Identifier if !m.partOfType && !m.partOfFun => {
        if (verbose) println("ident: " + m + ", prev = " + m.eprev)

        if (!m.eprev.isEmpty)
          if (verbose) println("prev type: " + m.eprev.get.etype)

        def searchfn(namespace : ScopeIdent) : Option[Resolvable] = {
          val variable = ScopeExpansions.vars.get(namespace)   orElse
                         ScopeExpansions.defs.getByGeneralName(namespace)   orElse
                         ScopeExpansions.enumsi.get(namespace) orElse
                         ScopeExpansions.enums.get(namespace)  orElse
                         getEnumInst(namespace)
          variable match {
            case Some(n@TypeParamAst(_,_)) if n.ordered && !(n.pos < m.pos) => None
            case _ => variable
          }
        }

        if (!m.eprev.isEmpty && m.eprev.get.etype.isEmpty) {
          SemanticError("could not resolve symbol because previous not found: " + m, m.pos)
        }

        // if no previous context to search, start with identifier's namespace
        val scope = if (m.eprev.isEmpty) m.escope.get.expandName else m.eprev.get.etype.get._1
        var binds = if (m.eprev.isEmpty) List() else m.eprev.get.etype.get._3

        var found = searchIdent(m.systemName, scope, searchfn)

        if (verbose) {
          println("\t resolve l-to-r: " + m + ", result = " + found)
        }

        if (found.isEmpty) {
          // skip super class if there is not a previous or an enclosing class
          if (!m.eprev.isEmpty || !m.ecls.isEmpty) {
            val gen = if (m.eprev.isEmpty) m.ecls.get.ident.toGeneric else m.eprev.get.etype.get._2
            val superResult = resolveSuperclass(m, m, searchfn, scope, gen, binds)

            if (!superResult.isEmpty) {
              found = superResult.get._1
              binds = binds ++ superResult.get._2
            }
          }
        }

        // now search for an enum extension type
        if (found.isEmpty && !m.eprev.isEmpty) {
          val t = m.eprev.get.eres.get._2 match {
            case m : TypeParamAst  => {
              m.param.eres.get._2 match {
                case m : EnumAst       => Some(m.param)
                case m : EnumInsideAst => Some(m.enum.get.param)
                case m : EnumInfAst    => Some(m.enum.get.param)
                case _                 => None
              }
            }
            case _                 => None
          }

          if (!t.isEmpty) {
            val scope = t.get.eres.get._1
            binds = t.get.resolution.get._3

            found = searchIdent(m.systemName, scope, searchfn)

            if (found.isEmpty) {
              val superResult = resolveSuperclass(m, m, searchfn, scope, t.get.resolution.get._2, binds)

              if (!superResult.isEmpty) {
                found = superResult.get._1
                binds = binds ++ superResult.get._2
              }
            }
          }
        }

        if (!found.isEmpty) {
          m.eres = found
          m.etype = Some(resolvableToInst(found.get._2, binds))
        } else {
          SemanticError("could not find symbol: " + m, m.pos)
        }
      }

      case m: AssignAst =>{
        val name = m.expr.eres.get._1;
        val found = ScopeExpansions.assigned.get(name)

        if(!found.isEmpty){
          if(found.get.assigned == true && found.get.dtype == false)
            SemanticError("val type cannot be reassigned", m.expr.get.pos)
          else(found.get.assigned == false)
            found.get.assigned = true
        }
      }

      case m : LitType if m.lit == "thisProxy" => {
        m.eres = m.x.eres
        m.etype = Some(resolvableToInst(m.eres.get._2, m.x.resolution.get._3))
      }

      case m : LitType => {
        m.eres = m.x.eres
        m.etype = Some(resolvableToInst(m.eres.get._2, List()))
      }

      case m : AsyncAst => {
        m.eres = m.expr.eres
        m.etype = m.expr.etype
      }

      case m : NewAst => {
        if (m.isProxyNew) {
          m.eres = m.expr.eres
          m.etype = m.expr.etype

          promoteProxyType(m, m.proxyExpr.size)
        } else {
          m.eres = m.expr.eres
          m.etype = m.expr.etype
        }
      }

      case m : DeleteAst => {
        m.eres = m.expr.eres
        m.etype = m.expr.etype
      }

      case m@BinOp(lst,".") => {
        if (verbose) println("binop: " + m)

        m.eres = lst.last.eres
        m.etype = lst.last.etype
      }

      case _ => ;
    }
  }

  def resolveDeclType(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : DeclAst if !m.expr.isEmpty => {
        val exprType = m.expr.get.etype.get._2
        val declType = m.tparam.param.generic.get
        val equal = Unifier.isEqual(declType,exprType)
        if (verbose) {
          println("resolveDeclType: checking type of: " + m)
          println("\t expression type: " + exprType)
          println("\t decl type: " + declType)
        }
        if (!equal) {
          SemanticErrorBin("types not same between expression and decl",
            m.tparam.param.pos, m.expr.get.pos)
        }
      }
      case _ => ;
    }
  }

  def checkReturnType(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ReturnAst => {
        // make sure the return type is "unit"
        if (m.expr.isEmpty) {
          // @TODO: check that it's unit, need system types to swallow basic types
        } else {
          val exprType = m.expr.get.etype.get._2
          val retType = m.edef.get.ret.toGeneric
          val equal = Unifier.isEqual(retType,exprType)
          if (verbose) {
            println("checkReturnType: checking type of: " + m)
            println("\t expression type: " + exprType)
            println("\t ret type: " + retType)
          }
          if (!equal) {
            SemanticErrorBin("types not same between return expression and def return type",
              m.edef.get.ret.pos, m.expr.get.pos)
          }
        }
      }
      case _ => ;
    }
  }

  def checkAssignType(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : AssignAst => {
        val exprType1 = m.expr.etype.get._2
        val exprType2 = m.expr2.etype.get._2
        val equal = Unifier.isEqual(exprType1,exprType2)
        if (verbose) {
          println("checkAssignType: checking type of: " + m)
          println("\t lval type: " + exprType1)
          println("\t rval type: " + exprType2)
        }
        if (!equal) {
          SemanticErrorBin("types not same between lval and rval",
            m.expr.pos, m.expr2.pos)
        }
      }
      case _ => ;
    }
  }

  def checkWhileCondition(x : AstTraversable, con : AstTraversable) : Unit = {
    val boolGeneric = basicRes("boolean")._2._2

    x match {
      case m : WhileAst => {
	val exprType = m.cond.get.etype.get._2;
        val equal = Unifier.isEqual(exprType, boolGeneric)
        if (verbose) {
          println("checkWhileCondition: " + m.cond)
          println("While Type is "+ exprType);
        }
        if (!equal) {
          SemanticError("condition in while should evaluate to boolean", m.cond.pos)
        }
      }
      case m : DoWhileAst => {
	val exprType = m.cond.get.etype.get._2;
        val equal = Unifier.isEqual(exprType, boolGeneric)
        if (!equal) {
          SemanticError("condition in do-while should evaluate to boolean", m.cond.pos)
        }
      }
      case _ => ;
    }
  }

  def checkIfCondition(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : IfAst => {
	val exprType = m.expr.get.etype.get._2;
        val boolGeneric = basicRes("boolean")._2._2
        val equal = Unifier.isEqual(exprType, boolGeneric)
        if (verbose) {
          println("checkIfCondition: " + m.expr)
          println("If Type is "+ exprType);
        }
        if (!equal) {
          SemanticError("if statement condition should evaluate to boolean", m.expr.pos)
        }
      }
      case _ => ;
    }
  }

  def checkForCondition(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ForAst => {
        for (c <- m.expr1) {
	  val exprType = c.etype.get._2;
          val boolGeneric = basicRes("boolean")._2._2
          val equal = Unifier.isEqual(exprType, boolGeneric)
          if (verbose) {
            println("checkForCondition: " + c)
          }
          if (!equal) {
            SemanticError("for statement condition should evaluate to boolean", c.pos)
          }
        }
      }
      case _ => ;
    }
  }

  def checkTemplateClass(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst if m.istemplate => {
        if (!m.parent.isEmpty) {
          SemanticError("template class can not inherit", m.pos)
        }
        if (m.ident.lst.size != 1) {
          SemanticError("template class must have exactly one generic parameter", m.ident.pos)
        }
      }
      case _ => ;
    }
  }

  def checkSetConstructor(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : TypeParamAst if m.set => {
        if (verbose) {
          println("checkSetConstructor: " + m)
        }
        val id = Identifier(List(m.str))
        id.ecls = m.ecls
        id.edef = m.edef
        id.escope = m.escope
        // resolve the symbol
        resolveIdentifier(id, id)
        // make sure that type is same with defined
        val declType = resolvableToType(id.eres.get._2).toGeneric
        val useType = m.param.toGeneric
        val equal = Unifier.isEqual(declType,useType)
        if (verbose) {
          println("checkSetConstructor: declType " + declType)
          println("checkSetConstructor: useType " + useType)
        }
        if (!equal) {
          SemanticErrorBin("types not same between constructor set and decl",
            m.param.pos, declType.t.pos)
        }
      }
      case _ => ;
    }
  }

  def checkAbstractChare(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : DefAst if !m.ecls.isEmpty && m.ecls.get.chare && m.ecls.get.abs && !m.lst.isEmpty => {
        SemanticErrorBin("abstract chare classes functions must be pure virtual", m.pos, m.ecls.get.pos)
      }
      case _ => ;
    }
  }

  def checkReturnChareDef(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : DefAst if !m.ecls.isEmpty && m.ecls.get.chare && m.entry && !m.cons &&
          m.ret.name.lst.head != "unit" && !(m.sync || m.async) => {
        SemanticErrorBin("chare classes entry defs must not have return type (unit)", m.pos, m.ecls.get.pos)
      }
      case _ => ;
    }
  }

  def checkExtendAbstractChare(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst if m.chare => {
        if (!m.parent.isEmpty) {
          val parentCls = m.parent.get.eres.get._2.asInstanceOf[ClassAst]
          if (!parentCls.abs) {
            SemanticErrorBin("parent classes of chares must be abstract", m.pos, parentCls.pos)
          }
          if (!parentCls.chare) {
            SemanticErrorBin("parent classes of chares must be chares", m.pos, parentCls.pos)
          }
        }
        for (t <- m.traits) {
          val traitCls = t.eres.get._2.asInstanceOf[ClassAst]
          if (!traitCls.abs) {
            SemanticErrorBin("trait classes of chares must be abstract", m.pos, traitCls.pos)
          }
          if (!traitCls.chare) {
            SemanticErrorBin("trait classes of chares must be chares", m.pos, traitCls.pos)
          }
        }
      }
      case _ => ;
    }
  }

  def checkDeclExpr(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : Identifier if !m.partOfType && !m.partOfFun =>
        m.eres.get._2 match {
          case n : TypeParamAst if !(n.pos < m.pos) =>
            SemanticErrorBin("var/val used before initialization\n", m.pos, n.pos)
          case _ => ;
        }
      case _ => ;
    }
  }

  def checkDeclInitalizer(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : DeclAst if m.edef == None && !m.expr.isEmpty => {
        m.expr.get.traverse(checkDeclExpr, null, null)
      }
      case _ => ;
    }
  }

  def checkEnums(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : EnumAst => {
        for (s <- m.inside) {
          s match {
            case n : EnumInsideAst => {
              if (!n.expr.isEmpty) {
                val exprType = n.expr.get.etype.get._2
                val declType = m.param.generic.get
                val equal = Unifier.isEqual(declType,exprType)
                if (verbose) {
                  println("checkEnums: exprType = " + exprType)
                  println("checkEnums: declType = " + declType)
                }
                if (!equal) {
                  SemanticErrorBin("types not same between declared 'type' and expression",
                    m.param.pos, n.expr.get.pos)
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
