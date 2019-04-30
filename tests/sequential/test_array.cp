include "basics.cp"

scope TestArray {
  def test() {
    val a : Array[int] = new Array[int](10);

    do {
      if (a[0] < 10) {
        a[0] = 10;
      }
    } while (false);

    for (var i : int = 0; i < a.size; i++) {
      if (a[i] < 10) {
        a[i] = 20*i;
      } else {
        a[i] = 10*i;
      }
    }
    for (var i : int = 0; i < a.size; i++) {
      println("i = " + i.toString() + ", a[i] = " + a[i].toString());
    }
  }
}

def main() {
  TestArray::test();
}
