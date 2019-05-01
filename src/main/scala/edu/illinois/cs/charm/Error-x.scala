package edu.illinois.cs.charm

import scala.util.parsing.input.Position

case class SemanticError(str : String, pos : Position) {
  println("[" + pos + "] failure: " + str + "\n\n" + pos.longString)
  System.exit(1)
}

case class SemanticErrorBin(str : String, pos : Position, pos2 : Position) {
  println("failure: " + str)
  println("[" + pos  + "]" + "\n\n" + pos.longString)
  println("[" + pos2 + "]" + "\n\n" + pos2.longString)
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
