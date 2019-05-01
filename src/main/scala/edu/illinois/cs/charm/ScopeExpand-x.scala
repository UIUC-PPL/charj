package edu.illinois.cs.charm

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.HashMap

class ScopeExpand(tree : AstTraversable, verbose : Boolean) {
  def start() {
    // create symbol table
    tree.traverse(begin, tree, end)
  }

  def begin(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst => {
        val name = m.expandName
        if (verbose) {
          println("ScopeExpand: " + m + ", name = " + name)
        }
        ScopeExpansions.classes.put(name,m)

        for (gen <- m.ident.lst) {
          // insert class scope with generic name tacked at the end
          val sident = gen.identifierScope
          ScopeExpansions.clsGeneric.put(sident,ClassGeneric(m,gen,m.istemplate))
          if (verbose) {
            println("declare generic: " + gen.name + ", scope = " + sident)
          }
        }
      }
      case m : EnumAst => {
        val name = ScopeIdent(m.expandName.scope.dropRight(1)) :+ m.str.name.lst.last
        if (verbose) {
          println("ScopeExpand: " + m + ", name = " + name)
        }

        if (!ScopeExpansions.enums.get(name).isEmpty) {
          SemanticErrorBin("name collision between enums", m.pos, ScopeExpansions.enums(name).pos)
        } else {
          ScopeExpansions.enums.put(name,m)
        }

        for (edecl <- m.inside) {
          edecl match {
            case n : EnumInsideAst => {
              val ename = n.enumScope
              if (verbose) {
                println("ScopeExpand: " + m + ", ename = " + ename)
              }
              if (!ScopeExpansions.enumsi.get(ename).isEmpty) {
                SemanticErrorBin("name collision between enums", m.pos, ScopeExpansions.enumsi(ename).pos)
              } else {
                ScopeExpansions.enumsi.put(ename,n)
              }
            }
            case n : EnumInfAst => {
              val ename = n.enumScope
              if (verbose) {
                println("ScopeExpand: " + m + ", ename = " + ename)
              }
              if (!ScopeExpansions.enumsx.get(ename).isEmpty) {
                SemanticErrorBin("name collision between enums", m.pos, ScopeExpansions.enumsx(ename).pos)
              } else {
                ScopeExpansions.enumsx.put(ename,n)
              }
            }
          }
        }
      }
      case m : DefAst => {
        val name = m.expandName

        if (verbose) {
          println("ScopeExpand: " + m + ", name = " + name)
        }

        for (gen <- m.ident.lst) {
          // insert class scope with generic name tacked at the end
          val sident = gen.identifierScope
          ScopeExpansions.defGeneric.put(sident,DefGeneric(m,gen))
          if (verbose) {
            println("declare function generic: " + gen.name + ", scope = " + sident)
          }
        }

        if (!ScopeExpansions.defs.get(name).isEmpty) {
          SemanticErrorBin("name collision between defs", m.pos, ScopeExpansions.defs(name).pos)
        } else {
          ScopeExpansions.defs.put(name,m)
        }
      }
      case m : Namespace => {
        val name = m.expandName
        if (verbose) {
          println("ScopeExpand: " + m + ", name = " + name)
        }

        // combine scopes that match
        if (!ScopeExpansions.scopes.get(name).isEmpty) {
          ScopeExpansions.scopes(name).slst ++= m.slst
          if (verbose) {
            println("ScopeExpand: " + m + ", name = " + name + ", combining with current")
          }
          m.folded = true
        } else {
          ScopeExpansions.scopes.put(name,m)
        }
      }
      case m : TypeParamAst => {
        val name = ScopeIdent(m.escope.get.expandName.scope :+ m.str)

        if (verbose) {
          println("ScopeExpand: " + m + ", TypeParamAst name = " + name)
        }

        if (!ScopeExpansions.vars.get(name).isEmpty) {
          val conflict = ScopeExpansions.vars.get(name).get
          SemanticErrorBin("symbol: " + m.str + " conflicts in same scope", m.pos, conflict.pos)
        } else {
          ScopeExpansions.vars.put(name,m)
        }
      }

      case m : DeclAst => {
        val name = ScopeIdent(m.tparam.escope.get.expandName.scope :+ m.tparam.str)
        if(!m.expr.isEmpty)
          m.assigned = true;
        ScopeExpansions.assigned.put(name,m);
       }

      case _ => ;
    }
  }

  def end(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m@Namespace(_,_) => if (m.folded) m.slst.clear()
      case _ => ;
    }
  }
}
