include "system.cp";
include "list.cp";
include "array.cp";
include "option.cp";

class Test[M] {
   val test : int = 4;
   def [](a : int) : int {
     return test + a;
   }
   def Test() {}
}

def main(args : Array[string]) {
  val xxx : int = 4;
  val lst : List[Ref[int]] = list(^4);
  lst.push(^xxx);
  val lst2 : List[Option[int]];
  val x : Some[int] = Some[int](3);
  lst2.push(x);
  lst2.push(Some[int](22));
  val mmm : Ref[Test[int]] = ^Test[int]();
  val nnn : int = (mmm#)[3];

  lst.foreach({ (r : Ref[int]) =>
    val x : int = r#;
  });

  lst.foreach({ (r : Ref[int]) =>
    val x : int = r#;

  });

  val lstx : List[int] = lst.map[int]({ (x : Ref[int]) : int =>
    return x#;
  });

  for (val i : int = 0; i < 10; i += 1) {
  }
}
