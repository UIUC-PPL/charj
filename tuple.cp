include "base.cp";
include "ref.cp";

class Tuple[A,B] {
  val _1 : A;
  val _2 : B;

  def Tuple(a_ : A, b_ : B) {
    _1 = a_;
    _2 = b_;
  }
}
def t2[A,B](a : A, b : B) : Tuple[A,B] { return Tuple[A,B](a,b); }

class Tuple3[A,B,C] : Tuple[A,B] {
  val _3 : C;

  def Tuple3(a_ : A, b_ : B, c_ : C) {
    _1 = a_;
    _2 = b_;
    _3 = c_;
  }
}
def t3[A,B,C](a : A, b : B, c : C) : Tuple3[A,B,C] { return Tuple3[A,B,C](a,b,c); }