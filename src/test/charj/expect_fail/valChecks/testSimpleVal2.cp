//This test has to fail because the val 'b' is reassigned
include "basics.cp"

def main(){
  var a : int;
  a = 2;
  a = 3;

  val b : int;
  b = 2;
  b = 3; //this statement causes compilation error
}
