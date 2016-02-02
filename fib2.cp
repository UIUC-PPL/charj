include "system.cp";

class [task] Fib {
  def Fib(n : int) : int {
    if (n > 1) {
      int x = async Fib(n-1);
      int y = async Fib(n-2);
      return sync(x) + sync(y);
    } else
      return n;
  }
}

def main() {
  val result : int = sync async Fib().compute(10);
}
