include "ref.cp";
include "base.cp";
include "seq.cp";

class Linked[T] {
  val item : Ref[T] = null;
  val next : Linked[T] = null;

  def Linked(cur_ : T, next_ : Linked[T]) {
    item = ^cur_;
    next = next_;
  }
}

class List[T] : Seq[T] {
  val front : Linked[T] = null;
  val back : Ref[T] = null;
  var size : int = 0;

  def List() { }
  def List(t : T) { push(t); }
  def List(front_ : Linked[T], back_ : Ref[T], curSize : int) {
    size = curSize;
    front = front_;
    back = back_;
  }

  // Seq interface
  def first() : T { return front.item#; }
  def last() : T { return back#; }

  def indexHelper(i : int, cur : int, lin : Linked[T]) : T {
    if (i == cur) return lin.item#;
    else return indexHelper(i, cur + 1, lin.next);
  }

  def index(i : int) : T { return indexHelper(i, 0, front); }
  def [](i : int) : T { return index(i); }

  def push(t : T) {
    if (back == null) back = ^t;
    front = Linked[T](t, front);
    size += 1;
  }

  def pop() : T {
    if (size == 1) {
      val ret : T = front.item#;
      front = null;
      back = null;
      return ret;
    } else if (size > 1) {
      val ret : T = front.item#;
      front = front.next;
    }
    size -= 1;
  }

  def tail() : List[T] {
    return List[T](front.next, back, size-1);
  }

  def foreach(fun : (T -> unit)) {
    var cur : Linked[T] = front;
    while (cur <> null) {
      fun(cur.item#);
      cur = cur.next;
    }
  }

  def map[M](fun : (T -> M)) : Seq[M] {
    var newLst : List[M] = List[M]();
    var cur : Linked[T] = front;
    while (cur <> null) {
      newLst.push(fun(cur.item#));
      cur = cur.next;
    }
    return newLst;
  }
}
def list[X](t : X) : List[X] { return List[X](t); }