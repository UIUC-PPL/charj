include "basics.cp"

val error_threshold : double = 0.004;

mainchare class Main {
  val prox : ProxyArray2[Jacobi];
  val rows : int, cols : int;
  val iters : int = 0;
  var maxIter : int = 100;

  entry def this(args : Array[string]) {
    if (args.size < 6) {
      println("usage: ./stencil <rows> <cols> <block-x> <block-y> <iters>");
      pexit();
    }

    rows = stoi(args[1]);
    cols = stoi(args[2]);
    val bx : int = stoi(args[3]);
    val by : int = stoi(args[4]);
    maxIter = stoi(args[5]);

    var debug : boolean = false;
    if (args.size == 7) debug = stoi(args[6]) == 1;

    println("arguments: " +
            "<rows=" + rows.toString() + "> " +
            "<cols=" + cols.toString() + "> " +
            "<block-x=" + bx.toString() + "> " +
            "<block-y=" + by.toString() + "> " +
            "<iters=" + maxIter.toString() + "> " +
            "<debug=" + debug.toString() + ">");

    prox = proxy(rows,cols) new Jacobi(thisProxy,rows,cols,bx,by,debug);
  }

  entry def finishIter(res : int) {
    iters++;

    if (iters == maxIter || res == 1) {
      println("finished iterations: " + iters.toString());
      pexit();
    } else {
      if (iters % 25 == 0)
        println("starting iteration " + iters.toString() + ", res = " + res.toString());
      prox.nextIter();
    }
  }
}

type Direction { Left, Right, Top, Bottom }

chare array2d class Jacobi {
  val mainProxy : Proxy[Main];
  var new_arr : Array2[double], old_arr : Array2[double];
  val debug : boolean;

  val rows : int, cols : int;
  val bx : int, by : int;

  val iter : int = 0;
  val is : int = 1,    js : int = 1;
  val ie : int = bx+1, je : int = by+1;

  val neighbors : int = 4;
  var count : int = 0;

  def this(=mainProxy : Proxy[Main],
           =rows : int, =cols : int,
           =bx : int, =by : int,
           =debug : boolean) {
    if (thisIndex.x == 0)      { is++; neighbors--; }
    if (thisIndex.y == 0)      { js++; neighbors--; }
    if (thisIndex.x == rows-1) { ie--; neighbors--; }
    if (thisIndex.y == cols-1) { je--; neighbors--; }

    new_arr = new Array2[double](bx+2, by+2);
    old_arr = new Array2[double](bx+2, by+2);

    for (var i : int = 0; i < bx+2; i++)
      for (var j : int = 0; j < by+2; j++)
        old_arr[i,j] = 0.0;

    if (debug) print_index("initialized, neighbors = " + neighbors.toString());

    constrainBoundary();

    nextIter();
  }

  def ~this() {
    delete new_arr;
    delete old_arr;
  }

  entry def ghost(in_iter : int, bound : Direction, arr : Array[double]) {
    count++;

    if (debug) 
      print_index("receiving ghost from: bound = " + bound.toString() +
                  ", count = " + count.toString() +
                  ", neighbors = " + neighbors.toString());

    if (bound == Direction::Left) {
      for (var j : int = 0; j < arr.size; j++)
        old_arr[0,j+1] = arr[j];
    } else if (bound == Direction::Right) {
      for (var j : int = 0; j < arr.size; j++)
        old_arr[bx+1,j+1] = arr[j];
    } else if (bound == Direction::Top) {
      for (var i : int = 0; i < arr.size; i++)
        old_arr[i+1,0] = arr[i];
    } else if (bound == Direction::Bottom) {
      for (var i : int = 0; i < arr.size; i++)
        old_arr[i+1,by+1] = arr[i];
    }

    if (count == neighbors) {
      count = 0;
      if (debug) print_index("calling kernel");
      val error : double = kernel();
      val conv : boolean = error < error_threshold;
      contribute(conv, Reduction::Logical_AND, mainProxy.finishIter(10));
    }
  }

  def print_index(str : string) {
    println("(" + thisIndex.x.toString() + "," + thisIndex.y.toString() + "): " + str);
  }

  entry def nextIter() {
    iter++;

    if (debug) print_index("sending boundaries: iter = " + iter.toString());

    if (thisIndex.x != 0) {
      val left : Array[double] = old_arr.sliceIRef(1,1,by+1);
      thisProxy[thisIndex.x-1, thisIndex.y].ghost(iter, Direction::Left, left);
      delete left;
    }
    if (thisIndex.x != rows-1) {
      val right : Array[double] = old_arr.sliceIRef(bx,1,by+1);
      thisProxy[thisIndex.x+1, thisIndex.y].ghost(iter, Direction::Right, right);
      delete right;
    }
    if (thisIndex.y != 0) {
      val top : Array[double] = old_arr.sliceJCopy(1,1,bx+1);
      thisProxy[thisIndex.x, thisIndex.y-1].ghost(iter, Direction::Top, top);
      delete top;
    }
    if (thisIndex.y != cols-1) {
      val bottom : Array[double] = old_arr.sliceJCopy(by,1,bx+1);
      thisProxy[thisIndex.x, thisIndex.y+1].ghost(iter, Direction::Bottom, bottom);
      delete bottom;
    }
  }

  def constrainBoundary() {
    if (thisIndex.x == 0) {
      for (var j : int = 0; j < by+2; j++) {
        old_arr[1,j] = 1.0;
        new_arr[1,j] = 1.0;
      }
    }
    if (thisIndex.x == rows-1) {
      for (var j : int = 0; j < by+2; j++) {
        old_arr[bx,j] = 1.0;
        new_arr[bx,j] = 1.0;
      }
    }
    if (thisIndex.y == 0) {
      for (var i : int = 0; i < bx+2; i++) {
        old_arr[i,1] = 1.0;
        new_arr[i,1] = 1.0;
      }
    }
    if (thisIndex.x == rows-1) {
      for (var i : int = 0; i < bx+2; i++) {
        old_arr[i,by] = 1.0;
        new_arr[i,by] = 1.0;
      }
    }
  }

  def kernel() : double {
    var diff : double = 0.0;
    var tempIth : double = 0.0;
    var max_err : double = 0.0;

    for (var i : int = is; i < ie; i++)
      for (var j : int = js; j < je; j++) {
        tempIth = 0.2 * (old_arr[i-1,j] + old_arr[i+1,j] + old_arr[i,j] + old_arr[i,j-1] + old_arr[i,j+1]);
        diff = tempIth - old_arr[i,j];
        if (diff < 0.0) diff = diff * minus(1.0);
        if (max_err <= diff) max_err = diff;
        new_arr[i,j] = tempIth;
      }

    println("max_err = " + max_err.toString());

    var prev_new : Array2[double] = new_arr;
    new_arr = old_arr;
    old_arr = prev_new;

    return max_err;
  }
}

