package CharjParser

object ClassEquality {
  import Checker._
  import BaseContext.verbose

  abstract class Direction
  case class Both() extends Direction
  case class LHS() extends Direction
  case class RHS() extends Direction

  def bindParentClass(cls : BoundClassSymbol) : BoundClassSymbol = {
    val parent = cls.cs.context.extensions(0)
    val pcls = parent.cs
    val bpcls = BoundClassSymbol(pcls, parent.bindings ++ cls.bindings)
    bpcls
  }

  // d is which sides we can move up in the class hierarchy (one side
  // might be allowed to be specialized or not)
  def equal(l : BoundClassSymbol, r : BoundClassSymbol, d : Direction = Both()) : Boolean = {
    val l1 = Unifier(true).subst(l.cs.t, l.bindings)
    val r1 = Unifier(true).subst(r.cs.t, r.bindings)
    if (verbose) println("check class equality: l1 = " + l1 + ", r1 = " + r1)
    if (!Unifier(false).isEqual(l1, r1)) {
      val lm1 = maybeResolveClass(Type(l1), null)
      val rm1 = maybeResolveClass(Type(r1), null)
      if (verbose) println("lm1 = " + lm1 + ", rm1 = " + rm1)
      if (!lm1.isEmpty && !rm1.isEmpty) {
        if (lm1.get.cs.level > rm1.get.cs.level && (d == LHS() || d == Both())) {
          if (verbose) println("l.level = " + lm1.get.cs.level + ", r.level = " + rm1.get.cs.level)
          val newl1 = bindParentClass(lm1.get)
          return equal(newl1, r, d)
        } else if (lm1.get.cs.level < rm1.get.cs.level && (d == RHS() || d == Both())) {
          if (verbose) println("l.cs = " + lm1.get.cs + ", l.cs.level = " + lm1.get.cs.level +
                               ", r.level = " + rm1.get.cs.level)
          val newr1 = bindParentClass(rm1.get)
          return equal(l, newr1, d)
        } else false
      } else false
    } else true
  }
}
