// Extensive rigor test

include "system.cp";
include "option.cp";

class Linked[T] {
  var cur : Ref[T];
  var next : Linked[T];

  def Linked(cur_ : Ref[T], next_ : Linked[T]) {
    cur = cur_;
    next = next_;
  }

  def getT() : Ref[T] {
    return cur;
  }
}

class ListTest[T] : SeqTest[T, Linked[T]] {
  var front : Ref[Linked[T]] = null;
  var back : Ref[Linked[T]] = null;
  var size : int = 0;

  def ListTest() { }

  def ListTest(front_ : Linked[T], back_ : Linked[T], curSize : int) {
    size = curSize;
    front = Ref[Linked[T]](front_);
    back = Ref[Linked[T]](back_);
  }

  def push(t : Ref[T]) {
    if (back == null) back = Ref[Linked[T]](Linked[T](t, null));
    front = Ref[Linked[T]](Linked[T](t, front.deref()));
    size += 1;
  }

  def pop() : Ref[T] {
    size -= 1;
    if (front.deref().cur == back.deref().cur) {
      var ret : Ref[T] = front.deref().cur;
      front = null;
      back = null;
      return ret;
    }
  }

  def hasNext() : boolean {
    if (!(cur.cur == null)) return true;
    else return false;
  }

  def next() : T {
    return cur.cur.deref();
  }

  def index(i : int) : Ref[T] { return indexHelper(i, 0, front.deref()); }

  def indexHelper(i : int, cur : int, item : Linked[T]) : Ref[T] {
    if (i == cur) return item.cur;
    else return indexHelper(i, cur + 1, item.next);
  }

  def last() : Ref[T] { return back.deref().cur; }
  def first() : Ref[T] { return back.deref().cur; }
  def head() : Ref[T] { return front.deref().cur; }
  def tail() : ListTest[T] { return ListTest[T](front.deref().next, back.deref(), size - 1); }
}

class SeqTest[T, Z] : Iterable[T, Z] {
  def last() : Ref[T];
  def first() : Ref[T];
  def index(i : int) : Ref[T];
}

class Iterable[T, U] {
  var cur : U;
  def hasNext() : boolean;
  def next() : T;
}

class Mapper[X, Y, U] : SeqTest[X, U] {
  def map(x : X) : Y;
  def method() {
    var a : Ref[X] = first();
  }
}

class MapElem[E1, E2] {
  var e1 : E1;
  var e2 : E2;
  
  def MapElem(e1_ : E1, e2_ : E2) { e1 = e1_; e2 = e2_; }
}

class Map[A, B] : SeqTest[A, MapElem[A, B]] {
  var lst : ListTest[MapElem[A, B]] = ListTest[MapElem[A, B]]();

  def Map() {}

  def find(a : A) : Option[B] {
    while (lst.hasNext()) {
      var elm : MapElem[A, B] = lst.next();
      if (elm.e1 == a) return Some[B](elm.e2);
    }
    return None[B]();
  }

  def hasNext() : boolean {
  }

  def next() : A {
  }

  def setVarue(a : A, b : B) {
    while (lst.hasNext()) {
      var elm : MapElem[A, B] = lst.next();
      if (elm.e1 == a) elm.e2 = b;
    }
  }

  def insert(a : A, b: B) {
    if (find(a) == None[B]()) {
      lst.push(Ref[MapElem[A, B]](MapElem[A, B](a, b)));
    } else {
      setVarue(a, b);
    }
  }

  def index(i : int) : Ref[A] {  }

  def last() : Ref[A] {  }
  def first() : Ref[A] {  }
}

class Driver[T] {
  def test111() {
    var xx : Map[int,unit] = Map[int,unit]();
    var ss : Some[int] = Some[int](xx.lst.next().e1);
    var mm : int = Ref[int](xx.lst.front.t.cur.t.e1).deref();
    var yy : Option[int] = ss;
    var zz : ListTest[int] = ListTest[int]();
    var nn : int = zz.front.deref().getT().deref();
  }

  def test() {
    var lst : ListTest[boolean] = ListTest[boolean]();
    var i : int = 10;
    var x : Ref[int] = Ref[int](i);
    var y : Ref[boolean] = Ref[boolean](true);
    //lst.push(x);
    lst.push(y);
    i += 10;
    var i2 : Ref[boolean] = lst.pop();
    var i3 : boolean = i2.deref();
  }
}

