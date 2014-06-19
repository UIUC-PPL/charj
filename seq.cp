include "base.cp";
include "ref.cp";

class Seq[T] {
   def first() : T;
   def last() : T;

   def foreach(fun : (T -> unit));
   def map[M](fun : (T -> M)) : Seq[M];
   //def take(n : int) : Seq[T];
   //def takeRight(n : int) : Seq[T];
   //def takeWhile(p : (T -> boolean)) : Seq[T];
}

class Indexable[I, T] : Seq[T] {
   def foreach_i(fun : (I -> unit));
   def [](x : I) : T;
}
class Indexable2[I, T] : Seq[T] {
   def foreach_i(fun : (I -> I -> unit));
   def [](x : I, y : I) : T;
}