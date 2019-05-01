include "basics.cp"

scope Option {
  abstract class Maybe[T] {
    def isEmpty() : boolean;
    def isFull() : boolean;
    def get() : T;
    def #() : T { return get(); }
  }

  class Some[T] : Maybe[T] {
    val t : T;
    
    def this(=t : T) {}
    def isEmpty() : boolean { return false; }
    def isFull() : boolean { return true; }
    def get() : T { return t; }
  }
  def $[M](t : M) : Some[M] { return new Some[M](t); }

  class None[T] : Maybe[T] {
    def isEmpty() : boolean { return true; }
    def isFull() : boolean { return false; }
    def get() : T { exit(123); }
  }
}
