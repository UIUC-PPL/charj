package FrontEnd

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ListBuffer,HashMap}

// Currently, contravariant and covariant type positions are not supported
// Assume invariant on type execept for container symbols

import Parse.verbose

abstract class Generic(val t : Type, val si : ScopeIdent) extends Positional {
  import Symbol._

  def <=(that : Generic) : Boolean = {
    if (verbose) {
      println("operator <= between this " + this + ", and that " + that)
      println("**<=: term1 = " + that)
      println("**<=: term2 = " + this)
    }

    if (Unifier.strictEqual(that,this)) return true

    def searchfn(namespace : ScopeIdent) : Option[Resolvable] = ScopeExpansions.classes.get(namespace)

    if (verbose) {
      println("searching for = " + that.t.name)
      println("searching in scope = " + that.si)
    }

    val found = searchIdent(that.t.name.systemName, that.si, searchfn)

    if (found.isEmpty) {
      return false
    }

    val curcls = found.get._2.asInstanceOf[ClassAst]
    val classDeclare = resolvableToType(found.get._2).toGeneric
    val binds = Unifier.unifyGeneric(classDeclare, that, List())

    if (verbose) {
      println("found = " + found)
      println("binds = " + binds)
    }

    var curfound : Boolean = false

    if (!curcls.parent.isEmpty) {
      val parentType = curcls.parent.get

      if (verbose) {
        println("operator <=: curcls = " + curcls)
        println("operator <=: parentType = " + parentType)
        println("operator <=: resolution = " + parentType.resolution)
      }

      // if the parent does not have a resolution which is required
      // for this step, unify it immediately
      if (parentType.resolution.isEmpty) {
        new ResolveSymbol(curcls.parent.get, true).unify(curcls.parent.get, null)
      }

      val resolution = parentType.resolution.get
      val parentUse = Unifier.subst(resolution._1, binds)

      if (verbose) println("operator <=: try search parent class = " + parentUse)

      curfound = this <= parentUse
    }

    if (curfound) {
      return curfound
    } else {
      for (t <- curcls.traits) {

        // if the trait does not have a resolution which is required
        // for this step, unify it immediately
        if (t.resolution.isEmpty) {
          new ResolveSymbol(t, true).unify(t, null)
        }

        val resolution = t.resolution.get
        val traitUse = Unifier.subst(resolution._1, binds)

        if (verbose) println("operator <=: try search trait class = " + traitUse)

        curfound = this <= traitUse

        if (curfound) return curfound
      }
    }

    return false
  }
}

case class FreeVar(str : ScopeIdent, t1 : Type) extends Generic(t1,str) {
  override def toString() = { "FreeVar(" + str + ")" }
}
case class FreeVarConstraint(str : ScopeIdent, cons : Generic, t1 : Type) extends Generic(t1,str) {
  override def toString() = { "FreeVarCC(" + str + " :> " + cons + ")" }
}
case class Bound(str : ScopeIdent, t1 : Type) extends Generic(t1,str) {
  override def toString() = { "Bound(" + str + ")" }
}
case class Container(str : ScopeIdent, lst : List[Generic], t1 : Type) extends Generic(t1,str) {
  override def toString() = { "Container(" + str + "," + lst + ")" }
}

object Unifier {
  type GenericBindings = List[(Generic, Generic)]

  def UnifySemanticError(str : String, pos : Position, pos2 : Position) : GenericBindings = {
    SemanticErrorBin(str, pos, pos2)
    List()
  }

  def unifyGenerics(a : Generic, b : Generic, terms1 : List[Generic], terms2 : List[Generic],
    subs : GenericBindings) : GenericBindings = {
    if (terms1.length != terms2.length)
      UnifySemanticError("unification failed: generic lengths not equal", a.t.pos, b.t.pos)
    else if (terms1.length == 0) subs
    else unifyGenerics(a, b, terms1.tail, terms2.tail, unifyGeneric(terms1.head, terms2.head, subs))
  }

  def isEqualGenerics(term1 : List[Generic], term2 : List[Generic]) : Boolean = {
    for ((x,y) <- ((term1,term2).zipped.toList))
      if (!isEqual(x,y)) return false
    true
  }

  def strictEqualGenerics(term1 : List[Generic], term2 : List[Generic]) : Boolean = {
    for ((x,y) <- ((term1,term2).zipped.toList))
      if (!strictEqual(x,y)) return false
    true
  }

  // equivalence relation where type hierarchy variation is not allowed
  def strictEqual(term1 : Generic, term2 : Generic) : Boolean = {
    if (verbose) {
      println("&& \t strictEqual: term1 = " + term1)
      println("&& \t strictEqual: term2 = " + term2)
    }

    ((term1, term2)) match {
      case (FreeVar(t1,_), FreeVar(t2,_)) => true //// TODO:: (this was already this way?)
      //case (FreeVar(_,_), FreeVarConstraint(_,_,_)) => true
      //case (FreeVarConstraint(_,_,_), FreeVar(_,_)) => true
      case (FreeVarConstraint(t1,c1,_), FreeVarConstraint(t2,c2,_)) => c1 == c2
      case (Bound(n1,_), FreeVarConstraint(t2,c2,_)) => c2 <= term1
      case (FreeVarConstraint(t1,c1,_),Bound(n2,_))  => c1 <= term2
      case (Container(_,_,_), FreeVarConstraint(_,c2,_)) => c2 <= term1
      case (FreeVarConstraint(_,c1,_),Container(_,_,_))  => c1 <= term2
      case (Container(n1,t1,_), Container(n2,t2,_)) if (n1 == n2) => strictEqualGenerics(t1,t2)

      // TODO: only one of these should be present??
      ///case (FreeVar(_,_), Bound(_,_)) => true
      case (Bound(_,_), FreeVar(_,_)) => true

      case (FreeVarConstraint(_,_,_), FreeVar(_,_)) => true
      case (Bound(n1,_), Bound(n2,_)) if n1 == n2 => true

      // check for enums
      case (Bound(n1,t1), Bound(n2,t2)) if (n1 != n2) && t1.einside && !t2.einside => {
        val nn1 = ScopeIdent(n1.scope.dropRight(1))
        val nn2 = n2
        nn1 == nn2
      }

      case _ => false
    }
  }

  // equivalence relation where type hierarchy variation is allowed in one term
  def isEqual(term1 : Generic, term2 : Generic) : Boolean = {
    if (verbose) {
      println("^^ \t isEqual: term1 = " + term1)
      println("^^ \t isEqual: term2 = " + term2)
    }

    ((term1, term2)) match {
      case (FreeVar(t1,_), FreeVar(t2,_)) => true //// ***TODO:: t1 == t2: is this correct
      case (FreeVarConstraint(t1,c1,_), FreeVarConstraint(t2,c2,_)) => isEqual(c1,c2)
      case (Bound(n1,_), FreeVarConstraint(t2,c2,_)) => c2 <= term1
      case (FreeVarConstraint(t1,c1,_),Bound(n2,_))  => c1 <= term2
      case (Container(_,_,_), FreeVarConstraint(_,c2,_)) => c2 <= term1
      case (FreeVarConstraint(_,c1,_),Container(_,_,_))  => c1 <= term2
      // TODO: order where more constrained is on the right
      case (FreeVarConstraint(_,_,_),FreeVar(_,_))  => true
      case (Container(n1,t1,_), Container(n2,t2,_)) if (n1 == n2 || term1 <= term2) => isEqualGenerics(t1,t2)
      case (Bound(n1,_), Bound(n2,_)) if (n1 == n2 || term1 <= term2) => true
      case (Bound(ScopeIdent(List(SystemIdentifier.namespace, "any")),_),_) => true
      case (_,Bound(ScopeIdent(List(SystemIdentifier.namespace, "any")),_)) => true
      case (FreeVar(_,_), Container(_,_,_)) => true
      case (FreeVar(_,_), Bound(_,_)) => true
      case _ => false
    }
  }

  def unifyGeneric(term1 : Generic, term2 : Generic, subs : GenericBindings) : GenericBindings = {
    if (verbose) {
      println("$$ \t UnifyGeneric: term1 = " + term1)
      println("$$ \t UnifyGeneric: term2 = " + term2)
    }

    ((term1, term2)) match {
      case (FreeVar(t1,_), FreeVar(t2,_)) if (t1 == t2) => subs
      case (FreeVarConstraint(t1,c1,_),FreeVarConstraint(t2,c2,_)) => unifyGeneric(c1,c2,subs)
      //case (FreeVarConstraint(t1,c1,_),FreeVar(t2,x)) => unifyVar(FreeVar(t2,x), term1, subs)
      case (FreeVar(t1,x),FreeVarConstraint(t2,c2,_)) => unifyVar(FreeVar(t1,x), term2, subs)
      case (FreeVarConstraint(t1,c1,x),_) => {
        if (verbose) {
          println("**FreeVarConstraint: term1 = " + c1)
          println("**FreeVarConstraint: term2 = " + term2)
        }

        if (!(c1 <= term2)) {
          UnifySemanticError("unification fail: type does not conform the constraint specification",
            term2.t.pos, c1.t.pos)
        }

        unifyVar(FreeVarConstraint(t1,c1,x), term2, subs)
      }
      case (_,FreeVarConstraint(t1,c1,x)) => {
        if (verbose) {
          println("**FreeVarConstraint: term1 = " + c1)
          println("**FreeVarConstraint: term2 = " + term1)
        }

        if (!(c1 <= term1)) {
          UnifySemanticError("unification fail: type does not conform the constraint specification",
            term1.t.pos, c1.t.pos)
        }

        unifyVar(FreeVarConstraint(t1,c1,x), term1, subs)
      }
      case (FreeVar(t1,x), _) => unifyVar(FreeVar(t1,x), term2, subs)
      case (_, FreeVar(t2,x)) => unifyVar(FreeVar(t2,x), term1, subs)
      case (Container(n1,t1,_), Container(n2,t2,_)) if (n1 == n2) => unifyGenerics(term1, term2, t1, t2, subs)
      case (Container(n1,t1,a), Container(n2,t2,b)) if (n1 != n2) =>
        UnifySemanticError("unification fail: containers not identical", a.pos, b.pos)
      case (Bound(n1,_), Bound(n2,_)) if (n1 == n2) => subs
      case _ => UnifySemanticError("types do not match: " + term1 + ", " + term2, term1.t.pos, term2.t.pos)
    }
  }

  def unifyVar(fvar : Generic, term2 : Generic, subs : GenericBindings) : GenericBindings = {
    if (occursCheck(fvar, term2, subs))
      UnifySemanticError("occurs check fails", fvar.t.pos, term2.t.pos)
    else
      subs.find{a => fvar == a._1} match {
        case Some((_,x)) => unifyGeneric(x, term2, subs)
        case None => {
          subs.find{a => term2 == a._1} match {
            case Some((_,x)) => unifyGeneric(fvar, x, subs)
            case None => (fvar,term2)::subs
          }
        }
      }
  }

  def occursCheck(fvar : Generic, t2 : Generic, subs : GenericBindings) : Boolean = {
    (fvar, t2) match {
      case (FreeVar(s1,_), FreeVar(s2,_)) if (s1 == s2) => true
      case (_, FreeVar(s1,_)) if (!subs.find{a => t2 == a._1}.isEmpty) =>
        occursCheck(fvar, subs.find{a => t2 == a._1}.get._2, subs)
      case (_, Container(_,terms,_)) => terms.map{x => occursCheck(fvar, x, subs)}.reduce(_ || _)
      case (_, _) => false
    }
  }

  def subst(term : Generic, subs : GenericBindings) : Generic = {
    (term, subs) match {
      case (_, List()) => term
      case (FreeVar(_,_), _) if (!subs.find(a => a._1 == term).isEmpty) => subst(subs.find(a => a._1 == term).get._2, subs)
      case (Container(s1,terms,x), _) => Container(s1,substAll(terms,subs),x)
      case (_, _) => term
    }
  }

  def substAll(terms : List[Generic], subs : GenericBindings) : List[Generic] = terms.map{t => subst(t, subs)}
}
