//This test has to fail because the val type object 'a' of the class abc is reassigned
//after it has been already initialized during its declartion

include "basics.cp"

class abc{
  val value : int = 0;
}

def main(){
  var a : abc = new abc();
  a = new abc();

  val b : abc = new abc();
  b = new abc(); //this statement causes compilation error
}
