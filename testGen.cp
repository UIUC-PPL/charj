include "system.cp";

class Base[T] {
  val x : int = 15;
  val mm : T;
  val mm1 : Base[double];

  def Base() {}
  def mytest() : Ref[Base[T]] {  }
}

class Parent  {
  val x : int = 15;

  def test2(t : int) {
    if (t == 10) {
      exit(20);
    }
  }
}

class Test : Parent {
  var x : int = 10;
  var y : int = 0;
  var xtf : Base[Base[Base[int]]] = Base[Base[Base[int]]]();
  var fy : Base[boolean];
  var fx : Base[int];

  def xx() {
    val xxxxxx : int = x + fx.mm + xtf.mm.mm.mytest()#.mytest().deref().mm;
  }
}
