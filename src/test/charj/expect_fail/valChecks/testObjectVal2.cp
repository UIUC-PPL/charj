//This test has to fail because the val type object 'a' of the class abc is reassigned
//after it has been already initialized in an assignment statment on line 12

include "basics.cp"

class abc{
  val value : int = 0;
}

def main(){
  var a : abc;
  a = new abc();
  a = new abc();


  val b : abc;
  b = new abc();
  b = new abc(); //this statement causes compilation error
}
