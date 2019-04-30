include "basics.cp"
include "future.cp"
include "option.cp"

import Option::Maybe

chare class Test {
  val x : int;

  entry def this(=x : int) { }

  async entry def asyncFun() : int {
    return x;
  }
}

mainchare class Main {
  val testProxy : Proxy[Test];

  entry def this(args : Array[string]) {
    testProxy = proxy new Test(42);
    thisProxy.regularFun();
  }

  entry def regularFun() : unit {
    val x : Future[int] = testProxy.asyncFun();
    var opt : Maybe[int] = x.probe();
    while (opt.isEmpty()) {
      opt = x.probe();
    }
    println("received value " + opt#.toString());
    pexit();
  }
}