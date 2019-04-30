include "basics.cp"

val y : int = 30;
val x : int = y + 10;

scope XY {
  val y : int = 20;
  val x : int = y + 10;
}

def main() {
  println("x = " + x.toString());
  println("y = " + y.toString());

  println("XY::x = " + XY::x.toString());
  println("XY::y = " + XY::y.toString());
}
