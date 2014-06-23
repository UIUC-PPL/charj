include "system.cp";

class ParArray2[T] : Array2[T] {
  def ParArray2(xdim : int, ydim : int) {
    sz1 = xdim;
    sz2 = ydim;
  }
  def reduce[T](fun : T -> T, fun2 : T -> unit) : T { }
}

class ParArray[T] : Array[T] {
  def ParArray(dim : int) {
    size = dim;
  }
  def reduce[T](fun : T -> T, fun2 : T -> unit) : T { }
}

