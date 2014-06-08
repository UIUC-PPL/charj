include "base.cp";
include "ref.cp";

class Test[T] {
   def method(a : T) : T;
}

class Test2[T] : Test[T] {
}

class Abstract[T] : Test2[T] {
   def method(a : int) : T {  }
}

class Concrete : Test2[int] {
   def method(a : int) : int { return 5; }
}