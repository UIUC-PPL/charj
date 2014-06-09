include "system.cp";

class Option[T] {
  def isEmpty() : boolean;
  def isFull() : boolean;
  def get() : T;
  def #() : T { return get(); }
}

class Some[T] : Option[T] {
  val t : T;
  
  def Some(t_ : T) { t = t_; }
  def isEmpty() : boolean { return false; }
  def isFull() : boolean { return true; }
  def get() : T { return t; }
}
def some[M](t : M) : Some[M] { return Some[M](t); }

class None[T] : Option[T] {
  def None() { }
  def isEmpty() : boolean { return true; }
  def isFull() : boolean { return false; }
  def get() : T { exitError("None has no item"); }
}