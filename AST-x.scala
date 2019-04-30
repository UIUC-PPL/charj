package FrontEnd

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ListBuffer,HashMap,Set}

import Parse.verbose

abstract class ProxType
case class NormProxy() extends ProxType
case class ProxyArray1() extends ProxType
case class ProxyArray2() extends ProxType
case class ProxyArray3() extends ProxType
case class ProxyElm() extends ProxType

trait Enclosing {
  var ecls : Option[ClassAst] = None
  var edef : Option[DefAst] = None
  var escope : Option[Scope] = None
  var eres : Option[(ScopeIdent,Resolvable)] = None
  var eprev : Option[AstTraversable] = None
  var etype : Option[(ScopeIdent, Generic, List[(Generic,Generic)])] = None
  var egen : String = ""
  var eprox : Option[ProxType] = None
  var ered : Boolean = false
  var alst : List[Annotation] = List[Annotation]()
}

trait Assignable {
  var assigned : Boolean = false
}

object UniqueID {
  var curId = 0
  def nextID : Int = {
    curId += 1
    curId
  }
}

object SystemIdentifier {
  val namespace = "&n"
}

// Resolvable is a trait that marks nodes in the AST that can be
// resolved based on type checking of a use
abstract trait Resolvable
abstract trait GenericSym { val t : Type }
case class ClassGeneric(cls : ClassAst, t : Type, templ : Boolean) extends Resolvable with GenericSym { }
case class DefGeneric(fun : DefAst, t : Type) extends Resolvable with GenericSym { }

case class ScopeIdent(scope : List[String]) {
  override def toString() = scope.reduceLeft[String](_ + "::" + _)

  // operator to append symbol to end of scope
  def :+(x : String) : ScopeIdent = ScopeIdent(scope :+ x)

  def generatePretty : String = toString().replace(":", "_").replace("&", "_")
}

// symbol tables for each kind
object ScopeExpansions {
  val scopes     : HashMap[ScopeIdent, Scope]         = new HashMap()
  val classes    : HashMap[ScopeIdent, ClassAst]      = new HashMap()
  val clsGeneric : HashMap[ScopeIdent, ClassGeneric]  = new HashMap()
  val defGeneric : HashMap[ScopeIdent, DefGeneric]    = new HashMap()
  val defs       : DefMap = new DefMap()
  val vars       : HashMap[ScopeIdent, TypeParamAst]  = new HashMap()
  val enums      : HashMap[ScopeIdent, EnumAst]       = new HashMap()
  val enumsi     : HashMap[ScopeIdent, EnumInsideAst] = new HashMap()
  val enumsx     : HashMap[ScopeIdent, EnumInfAst]    = new HashMap()
  val assigned   : HashMap[ScopeIdent, DeclAst]       = new HashMap()
}

class DefMap extends HashMap[ScopeIdent, DefAst] {
  // TODO come up with a better way to handle ambiguity
  def getByGeneralName(key : ScopeIdent) : Option[DefAst] = {
    // look for a function based on its non-mangled name
    val pairs = this.filter({ case (x, y) => {
      (y.ident.name.lst.head == key.scope.last) &&
        (key.scope.dropRight(1) == x.scope.dropRight(1))
    }})
    // if a single, umambiguous match was found, use it
    if (pairs.size == 1) Some(pairs.head._2)
    // otherwise... return None
    else None
  }
}

object AstTraversable {
  type TraverseFn[C] = ((AstTraversable, C) => Unit)
}
import AstTraversable._

abstract class AstTraversable extends Positional with Enclosing {
  val uid = UniqueID.nextID
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C])
  override def toString() = { "" }
  def get = this
}

abstract class Scope(sname : String, val slst : ListBuffer[AstTraversable], order : Boolean)
    extends AstTraversable
    with Resolvable {

  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    slst.map(l => l.traverse(fn_s,c,fn_e))
  }

  val parentScopes : ListBuffer[Scope] = ListBuffer()

  def getSpecificName : String

  def expandName : ScopeIdent = {
    if (parentScopes.size > 0)
      ScopeIdent(parentScopes.head.expandName.scope :+ getSpecificName)
    else
      ScopeIdent(List(getSpecificName))
  }
}

case class Namespace(name : String, lst : List[AstTraversable])
    extends Scope(name, lst.to[ListBuffer], false) {
  var folded = false
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    super.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "Namespace(" + name + "#" + uid + ")" }
  override def getSpecificName : String = name + SystemIdentifier.namespace
}

case class Identifier(lst : List[String]) extends AstTraversable {
  var partOfType = false
  var partOfFun = false

  def systemName = Identifier(lst.dropRight(1).map(_ + SystemIdentifier.namespace) :+ lst.last)
  def systemNameDef(params : Int) =
    Identifier(lst.dropRight(1).map(_ + SystemIdentifier.namespace) :+ (lst.last + "_" + params))

  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { lst.reduceLeft[String](_ ++ "::" ++ _) }
}

case class Annotation(id : String, params : List[AstTraversable] = List[AstTraversable]()) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }

  override def toString() = { s"Annotation(@$id($params)#$uid)" }
}

case class Type(name : Identifier, lst : List[Type], constraint : Option[Type])
    extends AstTraversable {
  var declare : Boolean = false
  var funcall : Boolean = false
  var outside : Boolean = false
  var isetype : Boolean = false
  var einside : Boolean = false
  var setpdef : Option[Int] = None
  var generic : Option[Generic] = None
  var istempl : Boolean = false
  var system  : Boolean = false

  // A resolution should only be present on non-declaratory types (type instantiations)
  // Option[(classUse, classDeclare, unification binidngs)]
  var resolution : Option[(Generic, Generic, List[(Generic,Generic)])] = None

  name.partOfType = true

  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    name.traverse(fn_s,c,fn_e)
    lst.map(l => l.traverse(fn_s,c,fn_e))
    if (!constraint.isEmpty) {
      constraint.get.traverse(fn_s,c,fn_e)
    }
    if(fn_e != null) fn_e(this,c)
  }

  def conflict(t : Type) : Boolean = name.lst.head == t.name.lst.head

  def setDeclare(x : Boolean) : Unit = {
    declare = x
    for (l <- lst) l.setDeclare(x)
  }

  def setOutside(x : Boolean) : Unit = outside = x

  // fancy printing for Type
  override def toString() = {
    val m = lst.map(_.toString)
    val g = if (m.size > 0) "[" + m.reduceLeft[String](_ ++ "," ++ _) + "]" else ""
    val con = if (constraint.isEmpty) "" else ":>" + constraint.get.toString()
    name.toString + g + con
  }

  def identifierScope : ScopeIdent =  {
    val encapScope = escope.get.expandName

    // if inside the type then we append on the identifier name
    if (outside) encapScope
    else encapScope :+ (name.lst.head + (if (setpdef.isEmpty) "" else "_" + setpdef.get))
  }

  // create the "Generic" the type that is used for abstractly
  // reasoning about types
  def toGeneric : Generic = {
    if (declare) {
      val iscope = identifierScope

      if (lst.size > 0) {
        Container(iscope, lst.map(x => x.toGeneric), this)
      } else if (!outside && isetype) {
        Bound(iscope, this)
      } else if (outside) {
        Bound(iscope, this)
      } else {
        if (constraint.isEmpty)
          FreeVar(iscope, this)
        else
          FreeVarConstraint(iscope, constraint.get.toGeneric, this)
      }
    } else {
      if (lst.size > 0) {
        Container(eres.get._1, lst.map(x => x.toGeneric), this)
      } else {
        eres.get._2 match {
          case x : GenericSym => {
            if (x.t.constraint.isEmpty)
              FreeVar(eres.get._1, this)
            else
              FreeVarConstraint(eres.get._1, x.t.constraint.get.toGeneric, this)
          }
          case x : EnumAst       => Bound(x.expandName, this)
          case x : EnumInsideAst => Bound(x.enumScope,  this)
          case _                 => Bound(eres.get._1, this)
        }
      }
    }
  }
}

case class ArrayDim(dim : Int) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "ArrayDim(" + dim + "#" + uid + ")" }
}

case class InternalGroup(raw : List[AstTraversable]) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    raw.map(_.traverse(fn_s,c,fn_e))
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "IGroup(" + raw + "#" + uid + ")" }
}

case class InternalNode(raw : String) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "INode(" + raw + "#" + uid + ")" }
}

case class ClassAst(istemplate : Boolean, istrait : Boolean, abs : Boolean,
                    mainchare : Boolean, chare : Boolean, charearray : Option[ArrayDim],
                    ident : Type, parent : Option[Type], traits : List[Type],
                    lst : ListBuffer[AstTraversable])
    extends Scope(ident.name.lst.head, lst.to[ListBuffer], false)
    with Resolvable {

  var level = -1
  val interface : ListBuffer[DefMeta] = ListBuffer()

  // excluding abstract methods
  val inheritMethods : Set[DefMeta]  = Set()
  val thisMethods    : Set[DefMeta]  = Set()

  // including abstract methods
  val thisMethodsAll    : Set[DefMeta]  = Set()
  val inheritMethodsAll : Set[DefMeta]  = Set()

  ident.lst.foreach(n => n.istempl = true)

  ident.setDeclare(true)
  ident.setOutside(true)

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    ident.traverse(fn_s,c,fn_e)
    parent match {
      case Some(x) => x.traverse(fn_s,c,fn_e)
      case _       => ;
    }
    traits.map(_.traverse(fn_s,c,fn_e))
    super.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "Class(" + ident.toString + "#" + uid + ")" }
  override def getSpecificName : String = ident.name.lst.head
}

// return type can change if this is a constructor def
case class DefAst(keywords : List[String],
                  reducer : Option[ReducerAst], ident : Type, params : List[TypeParamAst], var ret : Type,
                  lst : Option[List[AstTraversable]])
    extends Scope(ident.name.lst.head, if (lst.isEmpty) ListBuffer() else lst.get.to[ListBuffer], true)
    with Resolvable {
  def cons : Boolean = keywords contains "constructor"
  def destruct : Boolean = keywords contains "destructor"
  def entry : Boolean = keywords contains "entry"
  def system : Boolean = keywords contains "system"
  def sync : Boolean = keywords contains "sync"
  def async : Boolean = keywords contains "async"
  def threaded : Boolean =
    (keywords contains "threaded") || (callsSync.isDefined && callsSync.get)
  var callsSync : Option[Boolean] = None
  // this def is marked with override
  def over : Boolean = keywords contains "override"
  // this def is abstract due to no definition here
  val abs : Boolean = lst.isEmpty
  // old return type for constructor which changes for type checking
  var oldRet : Type = null
  // inheritied method
  var inherited : Option[DefAst] = None
  // optional control-flow information
  var cfg : Option[ControlFlow.Graph] = None

  ident.setpdef = Some(params.size)
  ident.setDeclare(true)

  // set params to be ordered so they must be used in lexigraphical order
  params.map(_.ordered = true)

  def toDefMeta : DefMeta = {
    val meta = DefMeta(ident.name, params.map(_.param.toGeneric), ret.toGeneric)
    meta.ecls = ecls
    meta.edef = this
    meta
  }

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(!reducer.isEmpty) reducer.get.traverse(fn_s,c,fn_e)
    ident.traverse(fn_s,c,fn_e)
    params.map(n => n.traverse(fn_s,c,fn_e))
    ret.traverse(fn_s,c,fn_e)
    if (oldRet != null) oldRet.traverse(fn_s,c,fn_e)
    super.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "Def(" + (if (abs) "*" else "") + ident.toString + "#" + uid + ")" }
  override def getSpecificName : String = ident.name.lst.head + "_" + params.size
}

case class WhenAst(entries : List[SdagEntryAst], lst : Option[List[AstTraversable]])
  extends Scope("when_" + (entries.map(_.ident.lst.head) mkString "_"),
                if (lst.isEmpty) ListBuffer() else lst.get.to[ListBuffer], true) {

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    entries.foreach(_.traverse(fn_s,c,fn_e))
    super.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }

  override def getSpecificName : String = "when_" + (entries mkString "_")
}

case class SdagEntryAst(ident : Identifier, private val params : List[AstTraversable])
    extends AstTraversable {

  val pattern : List[AstTraversable] =
    params.map(ptn => ptn match {
      case BinOp(List(Identifier(List("_"))),".") => PlaceholderAst()
      case _ => ptn
    })

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    // TODO determine if pattern and ident need to be visited
    if(fn_e != null) fn_e(this,c)
  }

  override def toString : String = s"""${ident.lst.head}(${pattern mkString ","})"""
}

case class PlaceholderAst() extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }

  override def toString : String = "_"
}

case class ReducerAst(params : List[Type]) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    params.map(n => n.traverse(fn_s,c,fn_e))
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "Reducer(" + params + ")#" + uid + "" }
}

case class TypeParamAst(str : String, param : Type) extends AstTraversable with Resolvable {
  // this declaratory typed identifier is ordered in its scope
  var ordered = false
  // is this a special type param (only for function params) that acts as an initializer
  var set = false

  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    param.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }

  override def toString() = { "TypeParam(" + str + "(" + param + ")" + "#" + uid + ")" }
}

case class IncludeAst(str : String) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }
}

case class BinOp(lhs : List[AstTraversable], op : String) extends AstTraversable {
  // slow way to build explicit links into list
  for (i <- 0 until lhs.size) {
    if (i != 0) {
      lhs(i).eprev = Some(lhs(i-1))
      if (verbose) {
        println("setting eprev: " + lhs(i) + " as " + lhs(i).eprev)
      }
    }
  }

  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    lhs.map(n => n.traverse(fn_s,c,fn_e))
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "BinOP(" + op + "," + lhs + "#" + uid + ")" }
}

case class FuncCall(var t : Type, params : List[AstTraversable]) extends AstTraversable {
  var superCall = false
  var oldSuperType : Type = null
  var skipGen = false

  t.funcall = true
  t.name.partOfFun = true

  // set reduction flag
  def setRed(x : AstTraversable, con : AstTraversable) { x.ered = true }
  if (t.name.lst.head == "contribute")
    params.foreach(_.traverse(setRed, null, null))

  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    t.traverse(fn_s,c,fn_e)
    params.map(n => n.traverse(fn_s,c,fn_e))
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "FuncCall(" + t + "," + params + "#" + uid + ")" }
}

case class ImportAst(lst : List[AstTraversable]) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    lst.map(_.traverse(fn_s,c,fn_e))
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "ImportAst(" + lst + "#" + uid + ")" }
}

case class ImportSpecificAst(lst : List[String]) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "ImportSpecificAst(" + lst + "#" + uid + ")" }
}

case class ImportAllAst(lst : List[String]) extends AstTraversable {
  def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "ImportAllAst(" + lst + "#" + uid + ")" }
}

// this class is temporarily used for saving positional context during parsing
case class PosString(str : String) extends Positional

case class DeclAst(dtype : Boolean, tparam : TypeParamAst, expr : Option[AstTraversable])
    extends AstTraversable
    with Resolvable with Assignable{

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    tparam.traverse(fn_s,c,fn_e)
    expr match {
      case Some(x) => x.traverse(fn_s,c,fn_e)
      case _       => ;
    }
    if(fn_e != null) fn_e(this,c)
  }

  override def toString() = { "Decl(" + tparam + (expr match {
    case Some(x) => " = " + x
    case None    => ""
  }) + "#" + uid + ")" }
}

case class ReturnAst(var expr : Option[AstTraversable]) extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    expr match {
      case Some(x) => x.traverse(fn_s,c,fn_e)
      case _       => ;
    }
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "Return(" + expr + "#" + uid + ")" }
}

case class AssignAst(expr : AstTraversable, expr2 : AstTraversable) extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    expr.traverse(fn_s,c,fn_e)
    expr2.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "AssignAst(" + expr + "=" + expr2 + "#" + uid + ")" }
}

case class LitType(lit : String, var x : Type) extends AstTraversable {
  var string = false

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    x.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "LitType(" + lit + "#" + uid + ")" }
}

case class IfAst(expr : AstTraversable, then : AstTraversable, el : Option[AstTraversable]) extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    expr.traverse(fn_s,c,fn_e)
    then.traverse(fn_s,c,fn_e)
    el match {
      case Some(x) => x.traverse(fn_s,c,fn_e)
      case _       => ;
    }
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "IfAst(" + expr + "#" + uid + ")" }
}

case class AsyncAst(expr : AstTraversable) extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    expr.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "AsyncAst(" + expr + "#" + uid + ")" }
}

case class NewAst(expr : AstTraversable, proxyExpr : List[AstTraversable]) extends AstTraversable {
  var isProxyNew : Boolean = false
  var templateCls : Option[ClassAst] = None

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    expr.traverse(fn_s,c,fn_e)
    proxyExpr.foreach(_.traverse(fn_s,c,fn_e))
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "NewAst(" + expr + "#" + uid + ")" }
}

case class DeleteAst(expr : AstTraversable) extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    expr.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "DeleteAst(" + expr + "#" + uid + ")" }
}

case class WhileAst(cond : AstTraversable, body : AstTraversable) extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    cond.traverse(fn_s,c,fn_e)
    body.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "WhileAst(" + cond + "#" + uid + ")" }
}

case class DoWhileAst(cond : AstTraversable, body : AstTraversable) extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    cond.traverse(fn_s,c,fn_e)
    body.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "DoWhileAst(" + cond + "#" + uid + ")" }
}

case class ForAst(decls : List[AstTraversable], expr1 : List[AstTraversable],
                  expr2 : List[AstTraversable], body : AstTraversable)
    extends AstTraversable {
  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    decls.map(_.traverse(fn_s,c,fn_e))
    expr1.map(_.traverse(fn_s,c,fn_e))
    expr2.map(_.traverse(fn_s,c,fn_e))
    body.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "ForAst(" + decls + "#" + uid + ")" }
}

case class EnumAst(str : Type, param : Type, inside : List[AstTraversable])
    extends Scope(str.name.lst.last, inside.to[ListBuffer], false)
    with Resolvable {

  str.setDeclare(true)
  str.setOutside(true)

  for (i <- inside) i match {
    case m : EnumInsideAst => m.enum = Some(this)
    case m : EnumInfAst    => m.enum = Some(this)
  }

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    str.traverse(fn_s,c,fn_e)
    param.traverse(fn_s,c,fn_e)
    inside.foreach(_.traverse(fn_s,c,fn_e))
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "EnumAst(" + str + "#" + uid + ")" }
  override def getSpecificName : String = str.name.lst.head + SystemIdentifier.namespace
}

case class EnumInsideAst(str : Type, expr : Option[AstTraversable])
    extends AstTraversable
    with Resolvable {

  var enum : Option[EnumAst] = None

  str.setDeclare(true)
  str.setOutside(false)
  str.isetype = true
  str.einside = true

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    str.traverse(fn_s,c,fn_e)
    expr match {
      case Some(x) => x.traverse(fn_s,c,fn_e)
      case _       => ;
    }
    if(fn_e != null) fn_e(this,c)
  }

  def enumScope : ScopeIdent = escope.get.expandName :+ str.name.lst.head

  override def toString() = { "EnumInsideAst(" + str + "#" + uid + ")" }
}

case class EnumInfInst(e : EnumInfAst, perm : String, inst : String)
    extends AstTraversable
    with Resolvable {

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    e.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }
  override def toString() = { "EnumInfInst(" + e + perm + inst + "#" + uid + ")" }
}

case class EnumInfAst(str : Type, lo : Option[String], hi : Option[String])
    extends AstTraversable
    with Resolvable {
  var enum : Option[EnumAst] = None

  str.setDeclare(true)
  str.setOutside(false)
  str.isetype = true
  str.einside = true

  override def traverse[C](fn_s : TraverseFn[C], c : C, fn_e : TraverseFn[C]) {
    if(fn_s != null) fn_s(this,c)
    str.traverse(fn_s,c,fn_e)
    if(fn_e != null) fn_e(this,c)
  }

  def enumScope : ScopeIdent = escope.get.expandName :+ str.name.lst.head

  override def toString() = { "EnumInfAst(" + str + "#" + uid + ")" }
}
