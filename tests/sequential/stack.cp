include "basics.cp"
include "list.cp"
include "option.cp"
include "container.cp"

scope Containers {
  class Node[T] : trait Nextable[T] {
    val item : T;
    val next : Node[T];

    // nextable interface
    def get() : T { return item; }
    def next() : Maybe[Node[T]] {
      if (next != null) return $(next); else return new None[Node[T]]();
    }

    def this(=item : T, =next : Node[T]) {}
  }

  abstract class Stack[T], trait HasToString, Iterable[T] {
    import Option::_

    var size : int = 0;

    def isEmpty() : boolean;
    def push(item : T);
    def pop() : T;
    def tryPop() : Maybe[T];

    def toString() : string { return "Stack"; }

    def iterator() : Iterator[T];
  }

  class ListStack[T] : Stack[T] {
    import Option::_

    var top : Node[T] = null;

    def this() {}
    def this(t : T) { push(t); }

    def push(item : T) {
      top = new Node[T](item, top);
      size += 1;
    }
    def isEmpty() : boolean {
      return top == null;
    }
    def tryPop() : Maybe[T] {
      if (isEmpty())
        return new None[T]();
      else
        return $pop();
    }
    def pop() : T {
      var oldTop : Node[T] = top;
      top = top.next;
      var it : T = oldTop.item;
        delete oldTop;
      size -= 1;
      return it;
    }

    def toString() : string { return "ListStack()"; }

    def iterator() : Iterator[T] {
      if (top != null) {
        return new InternalIterator[T,Node[T]]($top);
      } else {
        return new InternalIterator[T,Node[T]](new None[Node[T]]());
      }
    }
  }
}

import Containers::Stack

def addToStack(x : Stack[int], n : int) {
  x.push(10);
  x.push(15);
  x.push(n*5+n/2);
}

def pop_print(stack : Stack[HasToString], str : string) {
  val x : HasToString = stack.pop();
  val s : int = stack.size;
  println(str + ": item = " + x.toString() + ", size = " + s.toString());
}

def printUntilEmpty(stack : Stack[HasToString], str : string) {
  while (!stack.isEmpty()) {
    pop_print(stack, str);
  }
}

def main() {
  import Containers::ListStack

  var y : Stack[int] = new ListStack[int]();

  addToStack(y, 5);
  y.push(100);
  y.push(101);

  for (var i:int = 0, var j:int = 0; i < 10, j <= 10; i++, j += 2) {
    println("inserting: where i = " + i.toString() + ", j = " + j.toString() + ", i*j = " + (i*j).toString());
    y.push(i*j);
  }

  println("Stack = " + y.toString());
  println("Stack starting size = " + y.size.toString());

  val iter : Containers::Iterator[int] = y.iterator();
  while (iter.hasNext()) {
    println("testing stack iterator: " + iter.next().toString());
  }

  printUntilEmpty(y, "int stack");

  delete y;

  var str : Stack[string] = new ListStack[string]();
  str.push("test");
  str.push("test2");

  printUntilEmpty(str, "string stack");

  delete str;

  // use our stack to test some abstract things
  val testers : Stack[Tester] = new ListStack[Tester]();
  testers.push(new TestList());
  testers.push(new TestArray());
  testers.push(new TestComplex());

  val test_iter : Containers::Iterator[Tester] = testers.iterator();
  while (test_iter.hasNext()) {
    test_iter.next().test();
  }
}

abstract class Tester {
  def test();
}

// test the array
class TestArray : Tester {
  def test() {
    val a : Array[int] = new Array[int](10);
    for (var i : int = 0; i < a.size; i++) {
      a[i] = 10*i;
    }
    for (var i : int = 0; i < a.size; i++) {
      println("i = " + i.toString() + ", a[i] = " + a[i].toString());
    }
  }
}

// test the list
class TestList : Tester {
  import Containers::{Iterable,Iterator}

  def fib(n : int) : int {
    if (n < 2) return n;
    else return fib(n-1) + fib(n-2);
  }

  def printIter[T :> HasToString](x : Iterator[T]) {
    while (x.hasNext()) {
      println("iterator: " + x.next().toString());
    }
  }

  def insertFib(x : Containers::Seq[int], n : int) {
    if (n > 0) {
      x.push_front(fib(n));
      println("pushed value: " + fib(n).toString());
      insertFib(x, n-1);
    }
  }

  def test() {
    import Containers::{Seq,List,Iterator}

    val lst : Seq[int] = new List[int]();

    insertFib(lst, 20);

    println("index 10 = " + lst.get(12).toString());

    val iter : Iterator[int] = lst.iterator();
    printIter[int](iter);
    delete iter;
  }
}

// a simple complex number definition test
scope Num {
  class Complex : trait HasToString {
    val real : int;
    val img : int;

    def this(=real:int, =img:int){}

    def +(that : Complex) : Complex {
      // TODO: problem with leak here
      return new Complex(real+that.real,img+that.img);
    }

    def toString() : string {
      if (img >= 0)
        return real.toString() + " + " +  img.toString() + "i";
      else
        return real.toString() +  img.toString() + "i";
    }
  }
}

// test complex number which leaks memory
class TestComplex : Tester {
  import Num::_

  def test() {
    val x : Complex = new Complex(10, 4);
    val y : Complex = new Complex(8, -6);
    println("new complex = " + (x+y).toString());
  }
}
