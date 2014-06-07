class #string { }
class #char { }

class #int {
  def gen() : string { return "int32_t"; } 
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

