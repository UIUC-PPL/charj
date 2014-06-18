include "system.cp";
include "array.cp";
include "parallel.cp";
include "list.cp";

class Jacobi : ParArray2[Jacobi] {
  val block : Array2[double];
  val block_up : Array2[double];
  var localConv : boolean = false;
  var allConv : boolean = false;

  def Jacobi(xdim_ : int, ydim_ : int, bz1 : int, bz2 : int) {
    ParArray2[Jacobi](xdim_, ydim_);

    block = Array2[double](bz1+2,bz2+2);
    block_up = Array2[double](bz1+2,bz2+2);
    for (val i : int = 0; i < bz1+2; i += 1)
      for (val j : int = 0; j < bz2+2; j += 1)
        block[i,j] = 1.0;
  }

  def iteration(iter : int) {
    val left : Array[double] = block.column(0);
    val right : Array[double] = block.column(block.sz2-1);
    val top : Array[double] = block.row(0);
    val bottom : Array[double] = block.row(block.sz1-1);
    
    async this[wrap1(sz1-1),sz2].process(left, iter, { (b : Array2[double], arr : Array[double]) =>
      arr.foreach_i( {(j : int) => b[0,j+1] = arr[j];} );
    });
    async this[wrap1(sz1+1),sz2].process(right, iter, { (b : Array2[double], arr : Array[double]) =>
      arr.foreach_i( {(j : int) => b[block.sz2-1, j+1] = arr[j];} );
    });
    async this[sz1,wrap2(sz2-1)].process(bottom, iter, { (b : Array2[double], arr : Array[double]) =>
      arr.foreach_i( {(i : int) => b[i+1,0] = arr[i];} );
    });
    async this[sz1,wrap2(sz2+1)].process(top, iter, { (b : Array2[double], arr : Array[double]) =>
      arr.foreach_i( {(i : int) => b[i+1,block.sz1-1] = arr[i];} );
    });

    for (var i : int = 0; i < 4; i += 1)
      wait def process(arr : Array[double], it : int, fun : Array2[double] -> Array[double] -> unit)
        where it == iter { fun(block, arr); }

    localConv = kernel();
  }

  def kernel() : boolean {
    val maxdiff : double = 0.0;
    for (var i : int = 1; i < block.sz1; i += 1)
      for (var j : int = 1; j < block.sz2; j += 1) {
        block_up[i,j] = (block[i+1,j] + block[i-1,j] + block[i,j+1] + block[i,j-1] + block[i,j])*0.2;
        val diff : double = block_up[i,j] - block[i,j];
        if (diff > maxdiff) maxdiff = diff;
      }
    block = block_up;
    return maxdiff < 0.001;
  }

  def wrap1(i : int) : int { return (i+sz1) % sz1; }
  def wrap2(i : int) : int { return (i+sz2) % sz2; }

  def startReduction() {
    async this.reduce[boolean](
      {(cur : boolean) : boolean => return cur && localConv; },
      {(result : boolean) => allConv = result; startReduction();}
    );
  }

  def computeTillConverge() {
    var curIter : int = 0;
    var started : boolean = false;
    while (!allConv) {
      iteration(curIter);
      curIter += 1;
      if (!started) {
        startReduction();
        started = true;
      }
    }
  }

  // this should not be here but the wait is not semantically checked yet
  def process(arr : Array[double], i : int, fun : Array2[double] -> Array[double] -> unit) { fun(block, arr); }
}

def main(args : Array[string]) {
  val x : Jacobi = Jacobi(10, 10, 100, 100);
  sync (async x.computeTillConverge());
  exitProg(0);
}
