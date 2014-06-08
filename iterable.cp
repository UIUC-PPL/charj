include "base.cp";
include "ref.cp";

class Iterable[T, Item] {
  def newMonad() : Ref[Item];
  def atEnd(i : Ref[Item]) : boolean;
  def getCurrent(i : Ref[Item]) : T;
  def advance(i : Ref[Item]);
  def iterator() : Iterator[T, Item];
}

class Iterator[T, Item] {
  val it : Iterable[T, Item];
  val monad : Ref[Item];

  def Iterator(it_ : Iterable[T, Item]) {
    it = it_;
    monad = it_.newMonad();
  }

  def hasNext() : boolean {
    it.atEnd(monad);
  }
  def current() : T {
    it.getCurrent(monad);
  }
  def advance() {
    it.advance(monad);
  }
}