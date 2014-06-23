include "base.cp";

class Ref[T] {
  val t : T;
  def Ref(init : T) { t = init; }
  def deref() : T { return t; }
  def #() : T { return deref(); }
  def free() { }
}

// this is considered a special operator
def ^[M](t : M) : Ref[M] { return Ref[M](t); }
