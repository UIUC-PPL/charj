include "basics.cp"

mainchare class Main {
    var n : int;
    val arrProxy : ProxyArray[Hello];

    entry def this(args : Array[string]) {
        if (args.size >= 2) {
            n = stoi(args[1]);
        } else {
            n = 16;
        }
        println("[MAIN] Started on " + num_pes().toString() + " processors with " + n.toString() + " elements.");
        arrProxy = proxy(n) new Hello(thisProxy, n);
    }

    entry def initDone() {
        println("[MAIN] Initialization of array has finished.");
        arrProxy[0].sayHi(n);
    }

    entry def done() {
        println("[MAIN] All done.");
        pexit();
    } 
}

chare array1d class Hello {
    val mainProxy : Proxy[Main];
    val n : int;

    entry def this(=mainProxy : Proxy[Main], =n : int) {
        contribute(mainProxy.initDone());
    }

    entry def sayHi(m : int) {
        println("[HELLO#" + thisIndex.toString() + "] Hello from an array element!");
        m -= 1;
        if (m == 0) {
            mainProxy.done();
        } else {
            thisProxy[(thisIndex + 1) % n].sayHi(m);
        }
    }
}
