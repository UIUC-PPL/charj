include "basics.cp"

reducer[int] def int_max(x : int, y : int) : int {
  if (x >= y) {
    return x;
  } else {
    return y;
  }
}

mainchare class Main {
  val prox : ProxyArray[Test];

  entry def this(args : Array[string]) {
    prox = proxy(10) new Test(thisProxy);
  }

  entry def test(max : int) {
    println("test called with value " + max.toString());
    pexit();
  }
}

chare array1d class Test {
  val mainProxy : Proxy[Main];

  def this(=mainProxy : Proxy[Main]) {
    contribute(thisIndex, int_max, mainProxy.test(0));
  }
}

