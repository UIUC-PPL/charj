include "base.cp";
include "ref.cp";

class Test[T] {
   def method(a : T) : T;
   def method2(a : (T -> string)) : T;
}

class Test2[T] : Test[T] {
}

class Abstract[T] : Test2[T] {
   def method(a : int) : T {  }
}

class Concrete : Test2[int] {
   def method(a : int) : int { return 5; }
   def method2(a : (int -> string)) : int {
   
   }
}