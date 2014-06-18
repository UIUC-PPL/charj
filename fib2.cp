include "system.cp";
include "parallel.cp";

class Fib {
  def Fib() { }
  
  def compute(n : int) : int {
    if (n > 1)
      return sync(async Fib().compute() +
                  async Fib().compute(n-2));
    else
      return n;
  }
}

def main() {
  val result : int = sync(async Fib().compute(10));
}
