include "basics.cp"
include "msa.cp"

mainchare class Main {
  entry def this(args : Array[string]) {
    if (args.size < 2) {
      println("usage: ./test <n>");
      pexit();
    }

    val n    : int = stoi(args[1]);
    var arr  : MSA1[int] = new MSA1[int](n, n, 16*1024);
    val prox : ProxyArray[Test] = proxy(n) new Test(n, arr, thisProxy);

    prox.start();
  }

  entry def done() {
    pexit();
  }
}

chare array1d class Test {
  var arr  : MSA1[int];
  var prox : Proxy[Main];
  var n : int;

  entry def this(=n : int, =arr : MSA1[int], =prox : Proxy[Main]) { }

  @ignore
  entry def start() {
    println("[" + thisIndex.toString() + "] enrolling");
    arr.enroll(n);
    println("[" + thisIndex.toString() + "] syncing to write");
    arr.syncToWrite();
    println("[" + thisIndex.toString() + "] updating value");
    arr[thisIndex] = thisIndex;
    // arr[thisIndex] += thisIndex;
    println("[" + thisIndex.toString() + "] syncing to read");
    arr.syncToRead();
    @local println("[" + thisIndex.toString() + "] read " + arr[thisIndex].toString());
    contribute(prox.done());
  }
}
