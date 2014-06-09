include "system.cp";
include "parallel.cp";
include "option.cp";

class Fib : Chare  {
  val n : int;
  var total : int = 0;
  var counter : int = 0;
  var parent : Option[Fib];

  def Fib(n_ : int) { n = n_; }
  
  def compute(parent_ : Option[Fib]) {
    parent = parent_;
    if (n > 1) {
      async Fib(n-1).compute(some(this));
      async Fib(n-1).compute(some(this));
    } else {
      counter = 1;
      finished(n);
    }
  }
    
  def finished(n : int) {
    total += n;
    counter += 1;
    if (counter == 2) {
      if (parent.isEmpty())
        print(total);
      else
        async parent#.finished(total);
    }
  }
}

def main() {
  val f : Fib = Fib(10);
  async f.compute(None[Fib]());
}