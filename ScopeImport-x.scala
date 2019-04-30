package FrontEnd

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.Set

class ScopeImport(tree : AstTraversable, verbose : Boolean) {
  import Symbol._

  // symbols imported already
  val clsImport : Set[(ScopeIdent,ScopeIdent)] = Set()
  val defImport : Set[(ScopeIdent,ScopeIdent)] = Set()
  val varImport : Set[(ScopeIdent,ScopeIdent)] = Set()
  val enuImport : Set[(ScopeIdent,ScopeIdent)] = Set()

  def start() {
    tree.traverse(applyImport, tree, null)
  }

  def applyImport(x : AstTraversable, tree : AstTraversable) : Unit = {
    x match {
      case mm : ImportAllAst => {
        if (verbose) {
          println("ScopeImport (all): " + mm)
        }

        def searchfn_scope(search : ScopeIdent) : Option[Resolvable] = ScopeExpansions.scopes.get(search)

        val searchsym = mm.lst.dropRight(1).map(_ + SystemIdentifier.namespace)
        val found = searchIdent(Identifier(searchsym), mm.escope.get.expandName, searchfn_scope)

        if (found.isEmpty) {
          SemanticError("could not find scope", mm.pos)
        } else {
          if (verbose) {
            println("ScopeImport (all): found = " + found)
          }

          val scope = mm.escope.get.expandName

          found.get._2 match {
            case m : Namespace => {
              for (c <- m.slst) {
                if (verbose) println("ImportAll: try importing symbol: " + c)

                c match {
                  case n : ClassAst => {
                    val si = scope :+ n.ident.name.lst.last
                    val scopePair = (scope, n.escope.get.expandName :+ n.ident.name.lst.last)

                    if (verbose) println("ImportAll: trying to add: " + scopePair)

                    if (!(clsImport contains scopePair)) {
                      if (!ScopeExpansions.classes.get(si).isEmpty) {
                        SemanticErrorBin("imported class symbol conflicts with class in current scope",
                          m.pos, ScopeExpansions.classes.get(si).get.pos)
                      } else {
                        ScopeExpansions.classes.put(si,n)
                        clsImport += scopePair
                      }
                    }
                  }
                  case n : DeclAst => {
                    val si = scope :+ n.tparam.str
                    val scopePair = (scope, n.escope.get.expandName :+ n.tparam.str)

                    if (verbose) println("ImportAll: trying to add: " + scopePair)

                    if (!(varImport contains scopePair)) {
                      if (!ScopeExpansions.vars.get(si).isEmpty) {
                        SemanticErrorBin("imported class symbol conflicts with declaration in current scope",
                          m.pos, ScopeExpansions.vars.get(si).get.pos)
                      } else {
                        ScopeExpansions.vars.put(si,n.tparam)
                        varImport += scopePair
                      }
                    }
                  }
                  case n : EnumAst => {
                    val lname = n.str.name.lst.last
                    val si = scope :+ lname
                    val scopePair = (scope, n.escope.get.expandName :+ lname)

                    if (verbose) println("ImportAll: trying to add: " + scopePair)

                    if (!(enuImport contains scopePair)) {
                      if (!ScopeExpansions.enums.get(si).isEmpty) {
                        SemanticErrorBin("imported class symbol conflicts with enum declaration in current scope",
                          m.pos, ScopeExpansions.enums.get(si).get.pos)
                      } else {
                        ScopeExpansions.enums.put(si,n)
                        enuImport += scopePair

                        for (edecl <- n.inside) {
                          edecl match {
                            case o : EnumInsideAst => {
                              val encapScope = ScopeIdent(si.scope.dropRight(1))
                              val enumOutside = si.scope.last + SystemIdentifier.namespace
                              val nscope = encapScope :+ enumOutside

                              ScopeExpansions.enumsi.put(nscope :+ o.str.name.lst.last,o)
                            }
                          }
                        }
                      }
                    }
                  }
                  case n : DefAst => {
                    val si = scope :+ n.ident.name.lst.last + "_" + n.params.size
                    val scopePair = (scope, n.escope.get.expandName :+ n.ident.name.lst.last)

                    if (!(defImport contains scopePair)) {
                      if (!ScopeExpansions.defs.get(si).isEmpty) {
                        SemanticErrorBin("imported def symbol conflicts with def in current scope",
                          m.pos, ScopeExpansions.defs.get(si).get.pos)
                      } else {
                        ScopeExpansions.defs.put(si,n)
                        defImport += scopePair
                      }
                    }
                  }
                  case _ => ;
                }
              }
            }
          }
        }
      }
      case m : ImportSpecificAst => {
        if (verbose) {
          println("ScopeImport (specific): " + m + ", scope = " + m.escope)
        }

        def searchfn(search : ScopeIdent) : Option[Resolvable] =
          ScopeExpansions.classes.get(search) orElse
          ScopeExpansions.defs.get(search)    orElse
          ScopeExpansions.vars.get(search)    orElse
          ScopeExpansions.enums.get(search)

        val found = searchIdent(Identifier(m.lst).systemName, m.escope.get.expandName, searchfn)

        if (found.isEmpty) {
          SemanticError("could not find symbol designated in import", m.pos)
        } else {
          if (verbose) {
            println("ScopeImport (specific): found symbol = " + found + ", add to scope: " + m.escope.get.expandName)
          }

          // put it in the current scope where the import is written
          val scope = m.escope.get.expandName :+ m.lst.last
          val scopePair = (m.escope.get.expandName,found.get._1)

          found.get._2 match {
            case n : ClassAst => {
              if (verbose) println("ImportSpecificAst: trying to add: " + scopePair)

              if (!(clsImport contains scopePair)) {
                if (!ScopeExpansions.classes.get(scope).isEmpty) {
                  SemanticErrorBin("imported class symbol conflicts with class in current scope",
                    m.pos, ScopeExpansions.classes.get(scope).get.pos)
                } else {
                  ScopeExpansions.classes.put(scope,n)
                  clsImport += scopePair
                }
              }
            }
            case n : TypeParamAst => {
              if (verbose) println("ImportSpecificAst: trying to add: " + scopePair)

              if (!(varImport contains scopePair)) {
                if (!ScopeExpansions.vars.get(scope).isEmpty) {
                  SemanticErrorBin("imported decl symbol conflicts with decl in current scope",
                    m.pos, ScopeExpansions.vars.get(scope).get.pos)
                } else {
                  ScopeExpansions.vars.put(scope,n)
                  varImport += scopePair
                }
              }
            }
            case n : EnumAst => {
              if (verbose) println("ImportSpecificAst: trying to add: " + scopePair)

              if (!(enuImport contains scopePair)) {
                if (!ScopeExpansions.enums.get(scope).isEmpty) {
                  SemanticErrorBin("imported enum symbol conflicts with enum in current scope",
                    m.pos, ScopeExpansions.enums.get(scope).get.pos)
                } else {
                  ScopeExpansions.enums.put(scope,n)
                  enuImport += scopePair

                  for (edecl <- n.inside) {
                    edecl match {
                      case o : EnumInsideAst => {
                        val encapScope = ScopeIdent(scope.scope.dropRight(1))
                        val enumOutside = scope.scope.last + SystemIdentifier.namespace
                        val nscope = encapScope :+ enumOutside

                        ScopeExpansions.enumsi.put(nscope :+ o.str.name.lst.last,o)
                      }
                    }
                  }
                }
              }
            }
            case n : DefAst => {
              val scope = m.escope.get.expandName :+ (m.lst.last + "_" + n.params.size)

              if (!(defImport contains scopePair)) {
                if (!ScopeExpansions.defs.get(scope).isEmpty) {
                  SemanticErrorBin("imported def symbol conflicts with def in current scope",
                    m.pos, ScopeExpansions.defs.get(scope).get.pos)
                } else {
                  ScopeExpansions.defs.put(scope,n)
                  defImport += scopePair
                }
              }
            }
          }
        }
      }
      case _ => ;
    }
  }

}
