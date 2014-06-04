package CharjParser

object ClassEquality {
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
      if (l.cs.level > r.cs.level && (d == LHS() || d == Both())) {
        println("l.cs.level = " + l.cs.level + ", r.cs.level = " + r.cs.level)
        val newl1 = bindParentClass(l)
        return equal(newl1, r, d)
      } else if (l.cs.level < r.cs.level && (d == RHS() || d == Both())) {
        println("l.cs.level = " + l.cs.level + ", r.cs.level = " + r.cs.level)
        val newr1 = bindParentClass(r)
        return equal(l, newr1, d)
      } else false
    } else true
  }
}
