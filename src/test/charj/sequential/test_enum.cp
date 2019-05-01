include "basics.cp"

type TT { A, B, C }

scope XX {
  class ClassM[T] {
    val v : int;
    def this(=v : int) {}
    def method1() {}
  }
}

import XX::_
type Cls : ClassM[int] {
  X = new ClassM(10),
  Y = new ClassM(20),
  Z = new ClassM(30)
}

scope Test {
  type Direction : string {
    Left   = "left",
    Right  = "right",
    Top    = "top",
    Bottom = "bottom"
  }
  type MM { A, B, C }
}

type Ints : int { A_ 1 .. 10 }

def testa() {
  val x : Ints = Ints::A_1;
  val y : Ints::A_2 = Ints::A_2;
  val z : Ints::A_3 = Ints::A_3;

  if (x == y) {}
}

def test(a : TT) {
}

def main() {
  import Test::_

  val x : Direction = Direction::Right;
  val y : Direction::Left = Direction::Left;
  val z : Direction::Right = Direction::Right;

  val ym : Test::MM = Test::MM::A;
  ym.toString();

  test(TT::A);

  val m : Cls = Cls::X;
  val mm : int = m.v;
  m.method1();

  if (x == y) println("equal: x == y");
  if (x == z) println("equal: x == z");
}
