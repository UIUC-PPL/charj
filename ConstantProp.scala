package CharjParser

import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.token._
import scala.util.parsing._
import scala.util.parsing.combinator.lexical._
import scala.io._
import scala.language.postfixOps
import scala.util.parsing.input.Positional
import scala.collection.mutable.ListBuffer

class ConstantProp(tree : Stmt) {
  import BaseContext.verbose

  val found : ListBuffer[Expression] = ListBuffer()

  def start() { 
    new ExprVisitor(tree, examineStmt)
  }

  def examineStmt(expr : Expression, stm : Stmt) {
    expr match {
      case t@NumLiteral(_) =>
        println("numeric lit " + t + " in statement: " + stm)
        found += t
      case _ => ;
    }
  }
}
