//This test has to fail because the val array is reassigned
//However, the individual array elements can be modified
include "basics.cp"

def main(){
  var a : Array[int];
  a = new Array(10);
  a = new Array(3);

  val b : Array[int];
  b = new Array(10);
  b[0] = 10;
  b[0] = 10;
  b = new Array(3); //this statement causes compilation error
}
