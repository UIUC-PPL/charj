include "base.cp";
include "ref.cp";

class Seq[T] {
   def first() : T;
   def last() : T;

   def foreach(fun : (T -> unit));
   def map[M](fun : (T -> M)) : Seq[M];
}