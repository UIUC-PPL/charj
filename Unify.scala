package CharjParser

import scala.collection.mutable.{ListBuffer,ArrayBuffer}
import scala.util.parsing.input.Positional

abstract class Term extends Stmt {
  def getName : String
  def getTerms : List[Term]
}
case class Bound(t : String) extends Term {
  override def toString = "l\"" + t + "\""
  override def getName = t
  override def getTerms = List()
}
case class MVar(t : String) extends Term {
  override def toString = "?" + t
  override def getName = t
  override def getTerms = List()
}
case class Fun(n : String, terms : List[Term]) extends Term {
  override def toString = n + "[" + terms.foldRight("")((x,y) => x + "," + y) + "]"
  override def getName = n
  override def getTerms = terms
}
case class Tok(t : String) extends Term {
  override def toString = "UuU(" + t + ")"
  override def getName = t
  override def getTerms = List()
}

// @todo for future namespace support and resolution
case class Namespace(n : String, t : Term) extends Term {
  override def toString = "N_" + n + "(" + t + ")"
  override def getName = n
  override def getTerms = List(t)
}
case class MaybeNamespace(n : String, t : Term) extends Term {
  override def toString = "MN_" + n + "(" + t + ")"
  override def getName = n
  override def getTerms = List(t)
}

case class Unifier(mustUnify : Boolean) {
  var hasError : Boolean = false

  def UnifySemanticError(str : String) : List[(Term, Term)] = {
    if (mustUnify) SemanticErrorNone(str)
    hasError = true
    List()
  }

  def unifyTerms(terms1 : List[Term], terms2 : List[Term], subs : List[(Term, Term)]) : List[(Term, Term)] = {
    if (terms1.length != terms2.length) UnifySemanticError("unification fail: lengths not equal")
    else if (terms1.length == 0) subs
    else unifyTerms(terms1.tail, terms2.tail, unifyTerm(terms1.head, terms2.head, subs))
  }

  def isEqualTerms(term1 : List[Term], term2 : List[Term]) : Boolean = {
    for ((x,y) <- ((term1,term2).zipped.toList))
      if (!isEqual(x,y)) return false
    true
  }

  abstract class Exactness
  case class Exact() extends Exactness
  case class LHSVary() extends Exactness
  case class RHSVary() extends Exactness

  def isEqual(term1 : Term, term2 : Term, d : Exactness = Exact()) : Boolean = {
    ((term1, term2)) match {
      case (MVar(t1), MVar(t2)) => true //if (t1 == t2) => t1 == t2
      case (MVar(_), Bound(_)) if (d == LHSVary()) => true
      case (Bound(_), MVar(_)) if (d == RHSVary()) => true
      case (Namespace(n1,t1), Namespace(n2,t2)) if (n1 == n2) => isEqual(t1,t2)
      case (MaybeNamespace(n2,t1), MaybeNamespace(n1,t2)) => isEqual(t1,t2)
      case (Fun(n1,t1), Fun(n2,t2)) if (n1 == n2) => isEqualTerms(t1,t2)
      case (Bound(n1), Bound(n2)) if (n1 == n2) => true
      case _ => false
    }
  }

  def unifyTerm(term1 : Term, term2 : Term, subs : List[(Term, Term)]) : List[(Term, Term)] = {
    ((term1, term2)) match {
      case (MVar(t1), MVar(t2)) if (t1 == t2) => subs
      case (MVar(t1), _) => unifyVar(MVar(t1), term2, subs)
      case (_, MVar(t2)) => unifyVar(MVar(t2), term1, subs)
      case (Namespace(n1,t1), Namespace(n2,t2)) if (n1 == n2) => unifyTerm(t1, t2, subs)
      case (MaybeNamespace(n1,t1), Namespace(n2,t2)) if (n1 == n2) => unifyTerm(t1, t2, subs)
      case (Namespace(n1,t1), MaybeNamespace(n2,t2)) if (n1 == n2) => unifyTerm(t1, t2, subs)
      case (MaybeNamespace(n1,t1), t2) => unifyTerm(t1, t2, subs)
      case (t1, MaybeNamespace(n1,t2)) => unifyTerm(t1, t2, subs)
      case (Fun(n1,t1), Fun(n2,t2)) if (n1 == n2) => unifyTerms(t1, t2, subs)
      case (Fun(n1,t1), Fun(n2,t2)) if (n1 != n2) => UnifySemanticError("unification fail: functions not identical")
      case (Bound(n1), Bound(n2)) if (n1 == n2) => subs
      case (Bound(n1), Tok(n2)) if (n1 == n2) => subs
      case (Tok(n1), Bound(n2)) if (n1 == n2) => subs
      case (Bound(n1), Bound(n2)) if (n1 != n2) => UnifySemanticError("unification fail: bound vars not identical")
      case _ => UnifySemanticError("unification fail: terms don't match")
    }
  }

  def unifyVar(mvar : MVar, term2 : Term, subs : List[(Term, Term)]) : List[(Term, Term)] = {
    if (occursCheck(mvar, term2, subs)) UnifySemanticError("occurs check fails")
    else
      subs.find{a => mvar == a._1} match {
        case Some((_,x)) => unifyTerm(x, term2, subs)
        case None => {
          subs.find{a => term2 == a._1} match {
            case Some((_,x)) => unifyTerm(mvar, x, subs)
            case None => (mvar,term2)::subs
          }
        }
      }
  }

  def occursCheck(mvar : MVar, t2 : Term, subs : List[(Term, Term)]) : Boolean = {
    (mvar, t2) match {
      case (MVar(s1), MVar(s2)) if (s1 == s2) => true
      case (_, MVar(s1)) if (!subs.find{a => t2 == a._1}.isEmpty) =>
        occursCheck(mvar, subs.find{a => t2 == a._1}.get._2, subs)
      case (_, Fun(_,terms)) => terms.map{x => occursCheck(mvar, x, subs)}.reduce(_ || _)
      case (_, _) => false
    }
  }

  def subst(term : Term, subs : List[(Term, Term)]) : Term = {
    (term, subs) match {
      case (_, List()) => term
      case (MVar(_), _) if (!subs.find(a => a._1 == term).isEmpty) => subst(subs.find(a => a._1 == term).get._2, subs)
      case (Fun(s1,terms), _) => Fun(s1,substAll(terms,subs))
      case (Namespace(n1, t1), _) => Namespace(n1, subst(t1, subs))
      case (MaybeNamespace(n1, t1), _) => MaybeNamespace(n1, subst(t1, subs))
      case (_, _) => term
    }
  }

  def substAll(terms : List[Term], subs : List[(Term, Term)]) : List[Term] = terms.map{t => subst(t, subs)}
}
