include "basics.cp"

scope Fibonacci {
  abstract chare trait Responder {
    entry def result(n : int);
  }

  chare class Fib : trait Responder {
    var total : int = 0;
    var counter : int = 0;
    var parent : Proxy[Responder];

    entry def this(n : int, =parent : Proxy[Responder]) {
      if (n > 1) {
        proxy new Fib(n-1,thisProxy);
        proxy new Fib(n-2,thisProxy);
      } else {
        counter = 1;
        thisProxy.result(n);
      }
    }
    
    entry def result(n : int) {
      total += n;
      counter += 1;
      
      if (counter == 2)
        parent.result(total);
    }
  }
}

import Fibonacci::{Fib,Responder}

mainchare class Done : trait Responder {
  entry def this(args : Array[string]) {
    if (args.size < 2) {
      println("usage: ./fib <n>");
      pexit();
    }
    val n : int = stoi(args[1]);
    println("executing fib(" + n.toString() + ")");
    proxy new Fib(n, thisProxy);
  }
  entry def result(n : int) {
    println("result = " + n.toString());
    pexit();
  }
}
