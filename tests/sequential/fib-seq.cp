include "basics.cp"

scope Fibonacci {
  abstract trait Responder {
    def result(n : int);
  }

  class Fib : trait Responder {
    var total : int = 0;
    var counter : int = 0;
    var parent : Responder;

    def this(n : int, =parent : Responder) {
      if (n > 1) {
        new Fib(n-1,this);
        new Fib(n-2,this);
      } else {
        counter = 1;
        result(n);
      }
    }
    
    def result(n : int) {
      total += n;
      counter += 1;
      
      if (counter == 2)
        parent.result(total);
    }
  }
}

import Fibonacci::{Fib,Responder}

class Done : trait Responder {
  def result(n : int) {
    println("result = " + n.toString());
  }
}

def main(args : Array[string]) {
  if (args.size < 2) {
    println("usage: ./fib <n>");
    exit(1);
  }
  val n : int = stoi(args[1]);
  println("executing fib(" + n.toString() + ")");
  new Fib(n, new Done());
}
