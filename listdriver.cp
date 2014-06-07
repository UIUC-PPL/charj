include "system.cp";
include "list.cp";
include "option.cp";

def main() {
  val lst : List[Ref[int]] = list(^5);
  lst.push(^10);
  val lst2 : List[Option[int]];
  val x : Some[int] = Some[int](3);
  lst2.push(x);
  lst2.push(Some[int](22));
}

