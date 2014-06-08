include "ref.cp";
include "base.cp";
include "iterable.cp";

class ArrayCell {
  val cur : int;
  
  def ArrayCell(index : int) {
    cur = index;
  }
}

// this is a special system class for a raw block of data
class BLOCK[T] {
  def BLOCK(size : int) { }
  def [](index : int) : T { }
}

class Array[T] : Seq[T, ArrayCell] {
  val block : BLOCK[T];
  val size : int;

  def Array(size_ : int) {
    size = size_;
    block = BLOCK[T](size_);
  }

  def index(i : int) : T { return block[i]; }
  def [](i : int) : T { return index(i); }

  // Seq interface
  def first() : T { return index(0); }
  def last() : T { return index(size-1); }

  // Iterable interface
  def newMonad() : Ref[ArrayCell] {
    return ^ArrayCell(0);
  }
  def atEnd(i : Ref[ArrayCell]) : boolean {
    return (i#).cur == size-1;
  }
  def getCurrent(i : Ref[ArrayCell]) : T {
    return index((i#).cur);
  }
  def advance(i : Ref[ArrayCell]) {
    i.t = ArrayCell((i#).cur+1);
  }
  def iterator() : Iterator[T, ArrayCell] {
    return Iterator[T,ArrayCell](this);
  }
}

class ArrayCell2 {
  val cur1 : int;
  val cur2 : int;
  
  def ArrayCell2(index1 : int, index2 : int) {
    cur1 = index1;
    cur2 = index2;
  }
}


class Array2[T] : Seq[T, ArrayCell2] {
  val block : BLOCK[T];
  val sz1 : int;
  val sz2 : int;

  def Array2(sz1_ : int, sz2_ : int) {
    sz1 = sz1_;
    sz2 = sz2_;
    block = BLOCK[T](sz1_ * sz2_);
  }

  def index(i : int, j : int) : T { return block[i*sz1+j]; }
  def [](i : int, j : int) : T { return index(i,j); }

  // Seq interface
  def first() : T { return index(0,0); }
  def last() : T { return index(sz1-1,sz2-1); }

  // Iterable interface
  def newMonad() : Ref[ArrayCell2] {
    return ^ArrayCell2(0,0);
  }
  def atEnd(i : Ref[ArrayCell2]) : boolean {
    return (i#).cur1 == sz1-1 && (i#).cur2 == sz2-1;
  }
  def getCurrent(i : Ref[ArrayCell2]) : T {
    return index((i#).cur1,(i#).cur2);
  }
  def advance(i : Ref[ArrayCell2]) {
    if ((i#).cur2 <> sz2-1)
      i.t = ArrayCell2((i#).cur1,(i#).cur2+1);
    else
      i.t = ArrayCell2((i#).cur1+1,(i#).cur2);
  }
  def iterator() : Iterator[T, ArrayCell2] {
    return Iterator[T,ArrayCell2](this);
  }
}