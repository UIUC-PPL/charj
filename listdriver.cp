include "system.cp";
include "list.cp";

def main() {
  val lst : List[Ref[int]] = list(^5);
  lst.push(^10);
  //val lst2 : List[Option[int]];
  //lst2.push(Some[int](53));
}

