package CharjParser

import scala.util.parsing.input.Position

case class SemanticError(str : String, pos : Position) {
  println("[" + pos + "] Semantic error: " + str)
}
