package FrontEnd

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ListBuffer,HashMap}

abstract class MetaType

case class DefMeta(name : Identifier, params : List[Generic], ret : Generic)
    extends MetaType {

  import Parse.verbose

  var ecls : Option[ClassAst] = None
  var edef : DefAst = null

  def compare(that : DefMeta) : Boolean = {
    val plist = this.params zip that.params
    val pequal =
      if (plist.size > 0)
        plist.map(x => Unifier.strictEqual(x._1,x._2)).reduceLeft[Boolean](_ && _)
      else
        true
    val requal = Unifier.isEqual(this.ret, that.ret)
    if (verbose) {
      println("this.ret = " + this.ret + ", that.ret = " + that.ret)
    }
    pequal && requal
  }

  def instantiate(bindings : List[(Generic,Generic)]) : DefMeta = {
    val meta = DefMeta(name, params.map(Unifier.subst(_, bindings)), Unifier.subst(ret, bindings))
    meta.ecls = ecls
    meta.edef = edef
    meta
  }
}
