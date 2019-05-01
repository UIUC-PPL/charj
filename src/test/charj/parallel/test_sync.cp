include "basics.cp"
include "future.cp"

chare class Test {
  val x : int;

  entry def this(=x : int) { }

  sync entry def syncFun() : int {
    return x - 1;
  }

  async entry def asyncFun() : int {
    return x + 1;
  }
}

mainchare class Main {
  val testProxy : Proxy[Test];

  entry def this(args : Array[string]) {
    testProxy = proxy new Test(42);
    thisProxy.threadedFun();
  }

  entry def threadedFun() : unit {
    val x : int = testProxy.syncFun();
    println("received value " + x.toString());
    thisProxy.anotherThreadedFun();
  }

  entry def anotherThreadedFun() : unit {
    val x : Future[int] = testProxy.asyncFun();
    println("received value " + x#.toString());
    pexit();
  }
}