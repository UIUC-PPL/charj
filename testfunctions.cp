include "base.cp";
include "ref.cp";

class Test2[T] {
}

class Test[T] {
  def myfun(a : T -> string, m : string -> int -> Test2[T], x : T) : Test2[T] {
    return m(a(x),5);
  }

  def fun1(b : T -> string) {

  }

  def fun2(a : T) : string {

  }

  def xxx() {
    fun1(fun2);
  }
}