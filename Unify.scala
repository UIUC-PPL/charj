package CharjParser

import scala.collection.mutable.{ListBuffer,ArrayBuffer}
import scala.util.parsing.input.Positional

abstract class Term extends Stmt
case class Bound(t : String) extends Term { override def toString = "l\"" + t + "\"" }
case class MVar(t : String) extends Term { override def toString = "?" + t }
case class Fun(n : String, terms : List[Term]) extends Term { override def toString = n + "[" + terms + "]" }

object Unifier {
  def UnifySemanticError(str : String) : List[(Term, Term)] = {
    SemanticErrorNone(str)
    List()
  }

  def unifyTerms(terms1 : List[Term], terms2 : List[Term], subs : List[(Term, Term)]) : List[(Term, Term)] = {
    if (terms1.length != terms2.length) UnifySemanticError("unification fail: lengths not equal")
    else if (terms1.length == 0) subs
    else unifyTerms(terms1.tail, terms2.tail, unifyTerm(terms1.head, terms2.head, subs))
  }

  def unifyTerm(term1 : Term, term2 : Term, subs : List[(Term, Term)]) : List[(Term, Term)] = {
    ((term1, term2)) match {
      case (MVar(t1), MVar(t2)) if (t1 == t2) => subs
      case (MVar(t1), _) => unifyVar(MVar(t1), term2, subs)
      case (_, MVar(t2)) => unifyVar(MVar(t2), term1, subs)
      case (Fun(n1,t1), Fun(n2,t2)) if (n1 == n2) => unifyTerms(t1, t2, subs)
      case (Fun(n1,t1), Fun(n2,t2)) if (n1 != n2) => UnifySemanticError("unification fail: functions not identical")
      case (Bound(n1), Bound(n2)) if (n1 == n2) => subs
      case (Bound(n1), Bound(n2)) if (n1 != n2) => UnifySemanticError("unification fail: bound vars not identical")
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
      case (MVar(_), _) if (!subs.find(a => a._1 == term).isEmpty) => subs.find(a => a._1 == term).get._2
      case (Fun(s1,terms), _) => Fun(s1,substAll(terms,subs))
      case (_, _) => term
    }
  }

  def substAll(terms : List[Term], subs : List[(Term, Term)]) : List[Term] = terms.map{t => subst(t, subs)}
}
