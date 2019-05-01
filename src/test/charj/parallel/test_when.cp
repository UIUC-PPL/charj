include "basics.cp"
include "future.cp"
include "option.cp"

import Option::Maybe

mainchare class Main {
  val testProxy : Proxy[Test];

  entry def this(args : Array[string]) {
    testProxy = proxy new Test(thisProxy, 42);
    testProxy.sendToMain();
    thisProxy.waitOnValue();
  }

  entry def waitOnValue() {
    when receive(x : int) {
      println("received value " + x.toString());
      pexit();
    }
  }
}

chare class Test {
  val mainProxy : Proxy[Main];
  val x : int;

  entry def this(=mainProxy : Proxy[Main], =x : int) { }

  entry def sendToMain() {
    mainProxy.receive(x * 2);
  }
}
