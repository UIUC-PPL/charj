include "system.cp";
include "parallel.cp";

class Fib : Chare  {
  def Fib() { }
  
  def compute(n : int) : int {
    if (n > 1)
      return sync(async Fib().compute(n-1) +
                  async Fib().compute(n-2));
    else
      return n;
  }
}

def main() {
  val result : int = sync(async Fib().compute(10));
}


// def computeall(n : int) : int {
//     val total : int = 0;

//     async Fib().compute(n-1, this);
//     async Fib().compute(n-2, this);

//     wait recieveData(n : int) {
//       total += n;
//     }
// }

//with lambdas:

// def compute(n : int, fun : int => unit) : int {
//     val total : int = 0;

//     async Fib().compute(n-1, {n : int => total += n});
//     async Fib().compute(n-2, {n : int => total += n});
//     sync;
//
//     fun(total);
// }