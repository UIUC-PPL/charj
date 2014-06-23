package CharjParser

import scala.util.parsing.input.Position

case class SemanticError(str : String, pos : Position) {
  println("[" + pos + "] failure: " + str + "\n\n" + pos.longString)
  System.exit(1)
}

case class SemanticErrorNone(str : String) {
  println("[??] Semantic error: " + str)
  System.exit(1)
}

case class CodeGenError(str : String) {
  println("backend code generation error: " + str)
  System.exit(1)
}
