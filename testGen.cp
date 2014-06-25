include "system.cp";

class Base[T] {
  val x : int = 15;
  val mm : T;
  val mm1 : Base[double];

  def Base() {}
}

class Parent : Base[boolean] {
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
  var xtf : Base[int] = Base[int]();
  var fy : Base[boolean];
  var fx : Base[int];

  def test(x1 : int, y2 : int) : int {
    for (var i : int = 0, val j : int; i < 10 ; i+= 1) {
      val x3 : int = 20;
      x = 10;
    }

    test2(11);
  }

  def xxx(x : boolean) {
    var x2 : int;
    var x1 : int = 10 * 20 + x2;
    x1 = 20;

    x2 = test(10, 20);

    val y1 : boolean = true;

    if (!y1 && x1 < -x2 + 10) {
      val m1 : int = 10 + 20;
    } else {
      val m1 : Ref[int] = ^y;
      
    }

    while (x1 < x2) {
      x2 = 10;
    }

    test3[int, string](5);

    mmmxxx({ (x : boolean) : boolean => return x || y1; });
  }

  def mmmxxx(x : boolean -> boolean) {
    x(true);
  }

  def test3[M, N](t : M) {
    val x : M;
  }
}

