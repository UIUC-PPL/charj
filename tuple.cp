include "base.cp";
include "ref.cp";

class Tuple[A,B] {
  val _1 : A;
  val _2 : B;
}
def _[A,B](a : A, b : B) : Tuple[A,B] { return Tuple[A,B](a,b); }

class Tuple3[A,B,C] {
  val _1 : A;
  val _2 : B;
  val _3 : C;
}
def _[A,B,C](a : A, b : B, c : C) : Tuple3[A,B,C] { return Tuple3[A,B,C](a,b,c); }

