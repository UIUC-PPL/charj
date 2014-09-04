include "system.cp";
include "ref.cp";
include "tuple.cp";

class Node[T] {
  val item : T;
  val next : Ref[Node[T]];
}

class Stack[T]  {
  var size : int = 0;
  def isEmpty() : boolean;
  def push(item : T);
  def pop() : T;
}

class ListStack[T] : Stack[T] {
  var top : Ref[Node[T]] = null;
  def push(item : T) { top = new Node[T](item, top); size += 1; }
  def isEmpty() : boolean { return top == null; }
  def pop() : T {
    if (isEmpty()) exit(1);
    var oldTop : Ref[Node[T]] = top;
    top = top#.next;
    var it : T = oldTop#.item;
    oldTop.free();
    size -= 1;
    return it;
  }
}

def addToStack(x : Stack[int], n : int) {
  x.push(10);
  x.push(15);
  for (var i : int = 0; i < n; i += 1)
    for (var j : int = 0; j < n; j += 1)
      x.push(i*5+j);
}

def main() {
  var y : Stack[int] = ListStack[int]();
  var zz : Tuple[Stack[int], int] = _(y, 10);
  addToStack(zz._1, 5);
  print(zz._1.size);
  y.push(100);
  y.push(101);
  while (y.size > 0)
    print(y.pop());
}

