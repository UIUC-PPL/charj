include "base.cp";
include "ref.cp";

class Test2[T] {
}

class Test[T] {
  def myfun(a : T -> string, m : string -> int -> Test2[T], x : T) : Test2[T] {
    return m(a(x),5);
  }

  def fun1(b : T -> string)  {

  }

  def fun2(a : T) : string {

  }

  def xxx() {
    fun1(fun2);
  }

  def fun3(a : (T->string) -> (string -> T -> T)) : (string ->T ->T) {
    return a(fun2);
  }

  val mmm : int;



  def xx() : string {
    val x : (T -> string) -> unit = fun1;
    val y : T -> string = fun2;
    fun1(y);
    val t : T;
    val m : T -> string = { (a : T) : string =>
      val xxxxxxxx : int = 5;
      return "test";
    };

  } 

}