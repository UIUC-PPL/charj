include "system.cp";

class Fib {
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
  val result : int = sync async Fib().compute(10);
}
