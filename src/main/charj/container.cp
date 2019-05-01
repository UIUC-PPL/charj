include "basics.cp"
include "option.cp"

scope Containers {
  import Option::_

  abstract trait Iterator[T] {
    def hasNext() : boolean;
    def next() : T;
  }

  class InternalIterator[T, X :> Nextable[T]] : trait Iterator[T] {
    val cur : Maybe[X];
    def hasNext() : boolean {
      return !cur.isEmpty();
    }
    def next() : T {
      val x : T = cur#.get();
      cur = cur#.next();
      return x;
    }
    def this(=cur : Maybe[X]) {}
  }

  abstract trait Iterable[T] {
    def iterator() : Iterator[T];
  }

  abstract trait Indexable[T] {
    def get(i : int) : T;
  }

  abstract trait Seq[T] : trait Iterable[T], Indexable[T] {
    def size() : int;
    def push_front(item : T);
    def pop_front() : T;
  }

  abstract trait Nextable[T] {
    def get() : T;
    def next() : Maybe[Nextable[T]];
  }
}