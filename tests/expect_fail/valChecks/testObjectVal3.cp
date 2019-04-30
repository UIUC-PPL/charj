//This test has to fail because the val type data 'value of object 'a' is reassigned
//after it has been already initialized

include "basics.cp"

class abc{
  val value : int = 0;
  var variable : int = 4;
}

def main(){
  var a : abc;
  a = new abc();
  a = new abc();
  a.variable = 10;
  a.value = 10; //this statement causes compilation error
}
