package CharjParser

import scala.util.parsing.input.Position

case class SemanticError(str : String, pos : Position) {
  println("[" + pos + "] Semantic error: " + str)
  System.exit(1)
}

case class SemanticErrorNone(str : String) {
  println("[????] Semantic error: " + str)
  System.exit(1)
}
