include "basics.cp"

scope Test {
  class Base {}

  trait R00 {
    def x(a : int, b : int) { println("R00 x(" + (a+b).toString() + ")"); }
    def z() : int { return 80; }
  }

  trait R01 : trait R00 {
    def x(a : int, b : int) { println("R01 x("+ (a+b).toString() +")"); }
  }

  trait R02 : trait R00 {
    def x(a : int, b : int) { println("R02 x("+ (a-b).toString() +")"); }
  }

  class R1 : trait R02, R01 { }
}

def test1() {
  import Test::_
  val x : R02 = new R1();
  println("R1: cast as R02");
  x.x(10, 20);
}

def test2() {
  import Test::_
  val x : R1 = new R1();
  println("R1: cast as R1");
  x.x(10, 20);
}

def test3() {
  import Test::_
  val x : R01 = new R1();
  println("R1: cast as R01");
  x.x(10, 20);
}

def test4() {
  import Test::_
  val x : R00 = new R1();
  println("R1: cast as R00");
  x.x(10, 20);
}

def main() {
  import Test::_

  test1();
  test2();
  test3();
  test4();
}
