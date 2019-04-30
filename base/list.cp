include "basics.cp"
include "option.cp"

scope Containers {
  class Linked[T] : trait Nextable[T] {
    import Option::_

    val item : T;
    val next : Maybe[Linked[T]];

    def get() : T { return item; }
    def next() : Maybe[Linked[T]] { return next; }

    def this(=item : T, =next : Maybe[Linked[T]]) {}
  }

  abstract trait ListLike[T] {
    def head() : T;
    def tail() : ListLike[T];
  }

  // singly-linked list
  class List[T] : trait Seq[T], ListLike[T] {
    import Option::_

    var front : Maybe[Linked[T]];
    var cur_size : int;

    def this() {
      front = new None[Linked[T]]();
      cur_size = 0;
    }

    def head() : T { return front#.item; }

    def tail() : List[T] {
      val l : List[T] = new List[T]();
      l.cur_size = cur_size-1;
      l.front = front#.next;
      return l;
    }

    def get(i : int) : T {
      var cur : Maybe[Linked[T]] = front;
      var x : int = 0;
      for (; x < cur_size && x < i; x++) {
        cur = cur#.next;
      }

      assert(x < cur_size, "index out of range");

      if (x < cur_size) {
        return cur#.item;
      } else {
        return null;
      }
    }

    def size() : int { return cur_size; }

    def push_front(item : T) {
      front = $(new Linked[T](item, front));
      //println("pushing front size = " + front#.left().toString());
      cur_size += 1;
    }

    def pop_front() : T {
      val item : T = front#.item;
      val oldFront : Maybe[Linked[T]] = front;
      front = front#.next;
      delete oldFront#;
      delete oldFront;
      return item;
    }

    def iterator() : Iterator[T] {
      return new InternalIterator[T,Linked[T]](front);
    }
  }
}
