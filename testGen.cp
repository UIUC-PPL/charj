include "system.cp";

class Test {
  val x : int = 10;
  var y : int = 0;

  def test(x1 : int, y2 : int) : int {
    for (val i : int = 0, val j : int; i < 10 ; i+= 1) {
      val x3 : int = 20;
      x = 10;
    }
  }

  def xxx(x : boolean) {
    var x2 : int;
    var x1 : int = 10 * 20 + x2;
    x1 = 20;

    val y : boolean = true;

    if (y && x1 < x2 + 10) {
      val m1 : int = 10 + 20;
    } else {
      val m1 : int = 5;
      
    }

    while (x1 < x2) {
      x2 = 10;
    }
  }
}

