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

class Lexer extends StdLexical {
  override def token : Parser[Token] = floatTok | super.token

  def floatTok : Parser[Token] = { rep1(digit) ~ frac.? ^^
    { case lst ~ frac => NumericLit((lst mkString "") ++
                                   (if (frac.isEmpty) "" else frac.get)) }
  }
                                          
  def frac = {'.' ~ rep(digit) ^^
    { case dot ~ digits => "." ++ (digits mkString "") }
  }
}
