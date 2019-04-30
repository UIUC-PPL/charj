
trait HasToString {
  system def toString() : string { "std::to_string($me)"; return ""; }
}

system def -(that : int) : int { "-$that"; }
system def minus(that : double) : double { "-$that"; }
system def !(that : boolean) : boolean { "!$that"; }

trait NumOperators[T] {
  system def +(that : T) : T { "$me + $that"; }
  system def -(that : T) : T { "$me - $that"; }
  system def /(that : T) : T { "$me / $that"; }
  system def *(that : T) : T { "$me * $that"; }
  system def %(that : T) : T { "$me % $that"; }
  system def +=(that : T) : T { "$me = $me + $that, $me"; }
  system def -=(that : T) : T { "$me = $me - $that, $me"; }
  system def ++() : T { "$me = $me + 1, $me"; }
  system def --() : T { "$me = $me - 1, $me"; }
  system def <(that : T) : boolean { "$me < $that"; }
  system def >(that : T) : boolean { "$me > $that"; }
  system def <=(that : T) : boolean { "$me <= $that"; }
  system def >=(that : T) : boolean { "$me >= $that"; }
}

// manually override minus method for these because resolution will
// match the operator (not class method operator) first...
class int : trait NumOperators[int], HasToString {
  system def -(that : int) : int { "$me - $that"; }
}
class int64 : trait NumOperators[int64], HasToString {
  system def -(that : int64) : int64 { "$me - $that"; }
}
class int16 : trait NumOperators[int16], HasToString {
  system def -(that : int16) : int16 { "$me - $that"; }
}
class float : trait NumOperators[float], HasToString {
  system def -(that : float) : float { "$me - $that"; }
}
class double : trait NumOperators[double], HasToString {
  system def -(that : double) : double { "$me - $that"; }
}

class boolean : trait HasToString {
  system def &&(that : boolean) : boolean { "$me && $that"; }
  system def ||(that : boolean) : boolean { "$me || $that"; }
}

class unit {}
class any {}

class string : trait HasToString {
  system def +(that : string) : string {
    "std::string(std::to_string_base($me)) += std::string(std::to_string_base($that))";
  }
}

system def println(that : string) { "$that"; }
system def exit(that : int) { "exit($that)"; }
system def assert(that : boolean, str : string) { "assert($that && $str.c_str())"; }
system def stoi(that : string) : int { "atoi(std::to_char_arr($that))"; }

// dummy classes to keep this compiling
trait BLOCK[T]{}
trait BLOCK2[T]{}
trait BLOCK3[T]{}

// special array classes: fields "size*" and "b" are expected by the
// compiler
template class Array[T] {
  val owner : boolean;
  val size : int;
  val b : BLOCK[T];
  def this(=size : int) { }
  def [](i : int) : T { }
}
template class Array2[T] {
  val owner : boolean;
  val size1 : int;
  val size2 : int;
  val b : BLOCK2[T];
  def this(=size1 : int, =size2 : int) { }
  def [](i : int, j : int) : T { }
  def sliceIRef(i : int, j_lo : int, j_hi : int) : Array[T] { }
  def sliceICopy(i : int, j_lo : int, j_hi : int) : Array[T] { }
  def sliceJCopy(j : int, i_lo : int, i_hi : int) : Array[T] { }
}
template class Array3[T] {
  val owner : boolean;
  val size1 : int;
  val size2 : int;
  val size3 : int;
  val b : BLOCK3[T];
  def this(=size1 : int, =size2 : int, =size3 : int) { }
  def [](i : int, j : int, k : int) : T { }
}

// used to generate proxy types (thisProxy, etc.)
class Proxy[T] {
  val get : T;
}
class ProxyElm[T] {
  val get : T;
}
class ProxyArray[T] {
  val get : T;
  system def [](x : int) : ProxyElm[T] { "$me($x)"; }
}
class ProxyArray2[T] {
  val get : T;
  system def [](x : int, y : int) : ProxyElm[T] { "$me($x,$y)"; }
  system def destroy() { "$me.ckDestroy()"; }
}
class ProxyArray3[T] {
  val get : T;
  system def [](x : int, y : int, z : int) : ProxyElm[T] { "$me($x,$y,$z)"; }
}
// used to promote "proxy new X()" to a proxy type
system def make_proxy[T](x : T)        : Proxy[T]       { return null; }
system def make_proxy_array[T](x : T)  : ProxyArray[T]  { return null; }
system def make_proxy_array2[T](x : T) : ProxyArray2[T] { return null; }
system def make_proxy_array3[T](x : T) : ProxyArray3[T] { return null; }

system def my_pe() : int { "CkMyPe()"; }
system def pexit() { "CkExit()"; }

// classes for multi-dimensional chare arrays indexing
class Index2 {
  val x : int;
  val y : int;
}

class Index3 {
  val x : int;
  val y : int;
  val z : int;
}

system def contribute(x : unit) { ""; }
system def contribute(res : any, red : any, x : unit) { ""; }

type Reduction { Logical_AND, Logical_OR }
