include "basics.cp"

def main() {
  val x : boolean = false;
  val y : boolean = false;
  val z : boolean = false;

  val a : boolean = false;
  val b : boolean = false;
  val c : boolean = false;

  do {
    if (x) {
      y;
    }
  } while (z);

  for (x; y; z) {
    if (a) {
      b;
    } else {
      c;
    }
  }

  while (x) {
    if (y) {
      return;
    } else {
      z;
    }
  }
}
