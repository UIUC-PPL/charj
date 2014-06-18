class #string { }
class #char { }

class #int {
  def gen() : string { return "int32_t"; } 
}
class #double {
  def gen() : string { return "double"; }
}
class #float {
  def gen() : string { return "float"; }
}
class #int32 {
  def gen() : string { return "int32_t"; } 
}
class #int64 {
  def gen() : string { return "int64_t"; } 
}
class #boolean {
  def gen() : string { return "bool"; } 
}
class #unit {
  def gen() : string { return "void"; } 
}

