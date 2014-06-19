include "ref.cp";
include "base.cp";
include "seq.cp";

// this is a special system class for a raw block of data
class BLOCK[T] {
  def BLOCK(size : int) { }
  def [](index : int) : T { }
  def subblock(offset : int, newSize : int) : BLOCK[T] { }
}

class Array[T] : Indexable[int, T] {
  val block : BLOCK[T];
  val size : int;

  def Array(size_ : int) {
    size = size_;
    block = BLOCK[T](size_);
  }

  def Array(b : BLOCK[T], offset : int, size_ : int) {
    block = b.subblock(offset, size_);
    size = size_;
  }

  def index(i : int) : T { return block[i]; }
  def [](i : int) : T { return index(i); }

  // Seq interface
  def first() : T { return index(0); }
  def last() : T { return index(size-1); }

  def foreach(fun : (T -> unit)) {
    for (val i : int = 0; i < size; i += 1)
      fun(block[i]);
  }

  def foreach_i(fun : (int -> unit)) {
    for (val i : int = 0; i < size; i += 1)
      fun(i);
  }

  def takeWhile(p : (T -> boolean)) : Seq[T] {
    var count : int = 0;
    var found : boolean = true;
    for (val i : int = 0; i < size && found; i += 1)
      if (p(index(i))) count += 1;
      else found = false;
    val na : Array[T] = Array[T](count);
    for (val i : int = 0; i < size && found; i += 1)
      if (p(index(i))) na[i] = index(i);
    return na;
  }

  def map[M](fun : (T -> M)) : Seq[M] {
    var newArr : Array[M] = Array[M](size);
    for (val i : int = 0; i < size; i += 1) {
      newArr[i] = fun(block[i]);
    }
    return newArr;
  }
}

class Array2[T] : Indexable2[int, T] {
  val block : BLOCK[T];
  val sz1 : int;
  val sz2 : int;

  def Array2(sz1_ : int, sz2_ : int) {
    sz1 = sz1_;
    sz2 = sz2_;
    block = BLOCK[T](sz1_ * sz2_);
  }

  def row(i : int) : Array[T] {
    return Array[T](block, i*sz2, sz2);
  }

  def column(i : int) : Array[T] {
    val narr : Array[T] = Array[T](sz1);
    for (var j : int = 0; i < sz1; i += 1)
      narr[j] = this[i,j];
    return narr;
  }

  def index(i : int, j : int) : T { return block[i*sz1+j]; }
  def [](i : int, j : int) : T { return index(i,j); }

  // Seq interface
  def first() : T { return index(0,0); }
  def last() : T { return index(sz1-1,sz2-1); }

  def foreach(fun : (T -> unit)) {
    for (val i : int = 0; i < sz1; i += 1)
      for (val j : int = 0; j < sz2; j += 1)
        fun(this[i,j]);
  }

  def foreach_i(fun : (int -> int -> unit)) {
    for (val i : int = 0; i < sz1; i += 1)
      for (val j : int = 0; j < sz2; j += 1)
        fun(i, j);
  }

  def map[M](fun : (T -> M)) : Seq[M] {
    var newArr : Array2[M] = Array2[M](sz1,sz2);
    for (val i : int = 0; i < sz1; i += 1)
      for (val j : int = 0; j < sz2; j += 1)
        newArr[i,j] = fun(this[i,j]);
    return newArr;
  }
}