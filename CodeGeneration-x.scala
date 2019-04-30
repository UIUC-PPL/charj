package FrontEnd

import scala.util.parsing.input.{Positional,Position}
import scala.collection.mutable.{ListBuffer,HashMap}

// TODO fix code-generation for things like x.toString which recognizes toString as an identifier!

object ProxyType {
  def isProxyType(x : String) : Boolean =
    x == "Proxy" || x == "ProxyArray" || x == "ProxyArray2" || x == "ProxyArray3" || x == "ProxyElm"
  def toProxyType(x : String) : String = {
    if (x == "Proxy" || x == "ProxyArray" || x == "ProxyArray2" || x == "ProxyArray3") "CProxy_"
    else if (x == "ProxyElm") "CProxyElement_"
    else ""
  }
  def to_eprox(x : String) : ProxType = {
    if      (x == "Proxy")       NormProxy()
    else if (x == "ProxyArray")  ProxyArray1()
    else if (x == "ProxyArray2") ProxyArray2()
    else if (x == "ProxyArray3") ProxyArray3()
    else if (x == "ProxyElm")    ProxyElm()
    else                         NormProxy()
  }
  def ptypeToString(x : ProxType) : String = {
    x match {
      case ProxyElm() => "CProxyElement_"
      case _          => "CProxy_"
    }
  }
}

class Printer {
  var cur : StringBuilder = new StringBuilder()
  var indent = 0

  def <<(x : Int) : Unit = indent -= x
  def >>(x : Int) : Unit = indent += x
  def +=(x : String) : Unit = cur.append(x)
  def +:=(x : String) : Unit = {
    cur.append(List.fill(indent)("  ").foldRight("")(_+_))
    cur.append(x)
    cur.append("\n")
  }

  def finishci() {
    import java.io._
    val writer = new PrintWriter(Driver.outdir.resolve("generate_ci.cc").toFile)
    writer.write("#include \"interfaceBuilder.h\"\n")
    writer.write(cur.toString())
    writer.write("#include \"xi-main.h\"\n")
    writer.write("""
int main(int argc, char* argv[]) {
  xi::AstChildren<xi::Module>* ast = Builder::generateCI();
  processAst(ast, false, false, 0, 0, "generate", "");
}
""")
    writer.close()
  }

  def finish() {
    import java.io._
    val writer = new PrintWriter(Driver.outdir.resolve("generate.cc").toFile)
    writer.write("#include <cassert>\n")
    writer.write("#include <string>\n")
    writer.write("#include <iostream>\n")
    writer.write("#include <sstream>\n")
    writer.write("""
#if defined(__PARALLEL__)
  #include "charm++.h"
#endif
""")
    writer.write("""
#if defined(__MSA__)
#include "msa/msa.h"
#endif
""")
    writer.write("#include \"main.h\"\n");
    writer.write(cur.toString())
    writer.close()
  }
}

object BasicTypes {
  val templateGenSet : HashMap[String,String] = HashMap(
    "int" -> "int32_t",
    "int64" -> "int64_t",
    "int16" -> "int16_t",
    "float" -> "float",
    "double" -> "double",
    "boolean" -> "bool"
  )

  private val basics : HashMap[String,String] = HashMap(
    "unit" -> "void",
    "int" -> "int32_t",
    "int64" -> "int64_t",
    "int16" -> "int16_t",
    "float" -> "float",
    "double" -> "double",
    "string" -> "std::string",
    "boolean" -> "bool",
    "any" -> "Generic"
  )
  private val genericOut : HashMap[String,String] = HashMap(
    "unit" -> "xxx",
    "int" -> "i32",
    "int64" -> "i64",
    "int16" -> "i16",
    "float" -> "f",
    "double" -> "d",
    "string" -> "c",
    "boolean" -> "b",
    "any" -> "v"
  )
  private val lst : HashMap[ScopeIdent,String] =
    basics.map{x => (ScopeIdent(List(SystemIdentifier.namespace, x._1)), x._2) }
  private val lst_temp : HashMap[ScopeIdent,String] =
    templateGenSet.map{x => (ScopeIdent(List(SystemIdentifier.namespace, x._1)), x._1) }
  private val lst_gen : HashMap[ScopeIdent,String] =
    genericOut.map{x => (ScopeIdent(List(SystemIdentifier.namespace, x._1)), x._2) }

  def isBasicDecl(x : String) : Boolean = !basics.get(x).isEmpty
  def isBasic(x : ScopeIdent) : Boolean = !lst.get(x).isEmpty
  def basicOut(x : ScopeIdent) : String = lst.get(x).get

  def isTempBasic(x : ScopeIdent) : Boolean = !lst_temp.get(x).isEmpty
  def tempBasicOut(x : ScopeIdent) : String = lst_temp.get(x).get

  def isGen(x : ScopeIdent) : Boolean = !lst_gen.get(x).isEmpty
  def genOut(x : ScopeIdent) : String = lst_gen.get(x).get
}

object NameGenerator {
  private var counter = 0
  def createName : String = { counter += 1; "_gen_" + counter }
}

case class GenState(assign : Boolean, chareMode : Boolean)

object CodeGeneration {
  import BasicTypes._
  import NameGenerator._
  import ProxyType._

  def outputClassName(m : ClassAst, specialization : String = "") : String = {
    val scope = m.escope.get.expandName :+ m.ident.name.lst.head
    val genName = scope.generatePretty
    if (m.istemplate && specialization != "")
      genName + "_" + specialization
    else
      genName
  }

  // testTemplateType: return tuple (is_template_type, template_specialization, is_proxy_type)
  def testTemplateType(x : AstTraversable, isCons : Boolean = false, verbose : Boolean = false) : (Boolean, String, Boolean) = {
    var isTemplate : Option[ClassAst] = None

    if (!isCons) {
      val typ = Symbol.resolvableToType(x.eprev.get.eres.get._2)

      if (verbose) {
        println("testTemplateType: x = " + x + ", type = " + typ + ", x.eprev.get = " +
                x.eprev.get + ", eres = " +typ.eres)
      }

      if (!typ.eres.isEmpty) {
        typ.eres.get._2 match {
          case o : ClassAst if o.istemplate => isTemplate = Some(o)
          case o : EnumAst => return ((false, o.param.eres.get._1.generatePretty, false))
          case _ => ;
        }
      }

    } else {
      // hack to get out the template type without dancing on the edge of a pin!
      isTemplate = x.eprev.get.asInstanceOf[NewAst].templateCls
    }

    if (isTemplate.isEmpty) {
      val prevType = x.eprev.get.etype.get._2
      var is_proxy = false

      val prevOuterType = prevType match {
        // depromote proxy type
        case m : Container if isProxyType(m.str.scope.last) => is_proxy = true; m.lst.head.si
        case _ => prevType.si
      }

      if (verbose) {
        println("testTemplateType (no template):  prev type = " + x.eprev.get.etype.get._2
          + ", outer = " + prevOuterType + ", proxy = " + is_proxy)
      }

      return ((false, prevOuterType.generatePretty, is_proxy))
    } else {
      x.eprev.get.etype.get._2 match {
        // there must be exactly one generic in template type, thus Container
        case n : Container => {
          if (isTempBasic(n.lst.head.si)) {
            return ((true,outputClassName(isTemplate.get, tempBasicOut(n.lst.head.si)),false))
          } else {
            return ((false,outputClassName(isTemplate.get, ""),false))
          }
        }
      }
    }
  }
}

class CodeGeneration(tree : AstTraversable, verbose : Boolean) {
  import BasicTypes._
  import NameGenerator._
  import ProxyType._
  import CodeGeneration._

  val out   : Printer = new Printer()
  val outci : Printer = new Printer()

  val registerPUPables : ListBuffer[String] = ListBuffer()
  val registerInitCalls : ListBuffer[String] = ListBuffer()

  case class FoundSeqMain(var found : Boolean)

  def start() {
    tree.traverse(removeConstructorReturn, tree, null)
    tree.traverse(generateNonClsSig, tree, null)

    //
    // Sequential code generation
    //

    // find all the classes to generate
    val cls : ListBuffer[ClassAst] = ListBuffer()
    tree.traverse(findClassDecl, cls, null)
    // sort them by level
    for (c <- cls.sortWith(_.level < _.level)) {
      if (c.istemplate) {
        templateGenSet.map{x => generateClassDecl(c, x._1) }
        generateClassDecl(c)
      } else {
        generateClassDecl(c)
      }
    }

    // generate initalization function for all decls not in classes or
    // defs

    out +:= "void __seq_initalldecls()"
    out +:= "{"
    out>>1
    tree.traverse(generateDeclInit, null, null)
    out<<1
    out +:= "}"

    //
    // Parallel code generation
    //

    out +:= """#if defined(__PARALLEL__)"""

    // generate stubs for sequential funcs that might hook into 'main'
    tree.traverse(generateNonClsChareSig, tree, null)

    // generate stubs for all the abstract chare classes for virtual proxies
    tree.traverse(findAbstractCharesDecl, null, null)

    out +:= """#include "main_gen.decl.h""""

    out +:= "struct ValueMsg : public CMessage_ValueMsg {"
    out +:= "Generic value;"
    out +:= "};"

    tree.traverse(generateNonClsDefBody, tree, null)
    tree.traverse(generateClsDefBody, tree, null)

    out +:= "void __ch_registerAllPUPables();"

    // find all the abstract chare classes to generate virtual proxies
    tree.traverse(findAbstractChares, null, null)

    // find all the chare classes to generate
    val chareCls : ListBuffer[ClassAst] = ListBuffer()
    tree.traverse(findChareClass, chareCls, null)

    // skeleton for interface file
    outci +:= "namespace Builder {"
    outci>>1
    outci +:= "xi::AstChildren<xi::Module>* generateCI() {"
    outci>>1
    outci +:= "File* f = new File();"
    outci +:= """Module* m = new Module("main_gen", true);"""
    outci +:= """m->addModuleEntity(new InitCall("__ch_registerAllPUPables", 1));"""
    outci +:= """m->addModuleEntity(new InitCall("__seq_initalldecls", 1));"""

    // sort them by level
    for (c <- chareCls.sortWith(_.level < _.level)) {
      if (verbose) {
        println("generate chare class: " + c)
      }
      outputCharmInterface(c)
      outputCharmClassDecl(c)
    }

    outci +:= "#if defined(__MSA__)"
    for ((_,template) <- templateGenSet) {
      outci +:= "{"
      outci +:= """Array* c = new Array("MSA_PageArray", "1D");"""
      outci +:= """c->addTParam("MSA_DEFAULT_ENTRIES_PER_PAGE", true);"""
      outci +:= s"""c->addTParam("DefaultEntry<$template>");"""
      outci +:= s"""c->addTParam("$template", true);"""
      outci +:= """m->addModuleEntity(c);"""
      outci +:= "}"
      outci +:= "{"
      outci +:= """Group* c = new Group("MSA_CacheGroup");"""
      outci +:= """c->addTParam("MSA_DEFAULT_ENTRIES_PER_PAGE", true);"""
      outci +:= s"""c->addTParam("DefaultEntry<$template>");"""
      outci +:= s"""c->addTParam("$template", true);"""
      outci +:= """m->addModuleEntity(c);"""
      outci +:= "}"
    }
    outci +:= "#endif"

    for (s <- registerInitCalls) {
      outci +:= "{"
      outci +:= s"""InitCall* c = new InitCall("$s", true);"""
      outci +:= """m->addModuleEntity(c);"""
      outci +:= "}"
    }

    outci +:= "{"
    outci +:= """Message* c = new Message("ValueMsg");"""
    outci +:= """c->addMessageVar("Generic", "value");"""
    outci +:= """m->addModuleEntity(c);"""
    outci +:= "}"

    outci +:= "f->addModule(m);"
    outci +:= "return f->generateAst();"
    outci<<1
    outci +:= "}"
    outci<<1
    outci +:= "}"

    // output class bodies for chares
    tree.traverse(generateChareDefBody, tree, null)

    // // generate sequential funcs that might hook into 'main'
    // tree.traverse(generateNonClsChare, tree, null)

    /* Autogenerate Main chare */
//     outputChareInterfaceMain()
//     // output main chare code
//     out +:= """
// struct Main : public CBase_Main {
//   Main(CkArgMsg* msg) { _def__n__main_chare_(); }
// };
// """

    out +:= "void __ch_registerAllPUPables()"
    out +:= "{"
    out>>1
    for (s <- registerPUPables) {
      out +:= s"PUPable_reg($s);"
    }
    out<<1
    out +:= "}"

    out +:= """#include "main_gen.def.h""""

    out +:= "#else /* __PARALLEL__ */"

    val x = FoundSeqMain(false)
    tree.traverse(findMainFun, x, null)

    if (!x.found) {
      out +:= """
int main() {
  fprintf(stderr, "Program must be compiled and run with Charm++\n");
  return 1;
}"""
    }

    out +:= """#endif /* __PARALLEL__ */"""

    // write it all out to file
    outci.finishci()
    out.finish()
  }

  def findMainFun(x : AstTraversable, f : FoundSeqMain) {
    x match {
      case m : DefAst if m.ident.name.lst.head == "main" && m.ecls.isEmpty => {
        if (m.params.size == 0) {
          out +:= """int main() { __seq_initalldecls(); _def__n__main(); return 0; }"""
        } else if (m.params.size == 1) {
          out +:= """
int main(int argc, char** argv) {
  Generic MAIN_gen_1 = Generic((void*)(new _n__Array()));
  ((_n__Array*)(MAIN_gen_1.t.v))->_def_this(argc);
  for (int i = 0; i < argc; i++) {
    ((_n__Array*)(MAIN_gen_1.t.v))->_def__LBRACK__RBRACK_(i) = std::string(argv[i]);
  }
  __seq_initalldecls();
  _def__n__main(MAIN_gen_1);
  return 0;
}"""
        }
        f.found = true
      }
      case _ => ;
    }
  }

  def removeConstructorReturn(x : AstTraversable, con : AstTraversable) {
    x match {
      case m : DefAst if m.cons => {
        // swap oldRet and ret
        val ret = m.ret
        m.ret = m.oldRet
        m.oldRet = ret
      }
      case _ => ;
    }
  }

  def declFunction(markVirtual : Boolean, retType : String, fnName : String,
                   params : List[String], addSemi : Boolean, pure : Boolean) {
    val p = if (params.size == 0) "" else params.reduceLeft[String](_ + "," + _)
    val virtual = if (markVirtual) "virtual " else ""
    out +:= virtual + retType + " " + fnName + "(" + p + ")" + (if (pure) " = 0" else "") + (if (addSemi) ";" else "")
  }

  def outputTypeInner(si : ScopeIdent) : String = if (isBasic(si)) basicOut(si) else "void*"
  def outputType(si : ScopeIdent) : String = if (isBasic(si)) basicOut(si) else "Generic"

  def outputType(x : Type, chareMode : Boolean) : String = {
    if (!x.resolution.isEmpty) {
      val res = x.resolution.get._1
      res match {
        case m : Container if chareMode && isProxyType(m.str.scope.last) => {
          val inner = m.lst.head match {
            case n : FreeVarConstraint => n.cons.si.generatePretty
            case _                     => m.lst.head.si.generatePretty
          }
          if (verbose) {
            println("outputType: PROXY: " + x.resolution)
          }
          val ptype = toProxyType(m.str.scope.last)
          s"$ptype$inner" + "_chare_"
        }
        case _ => outputType(res.si)
      }
    } else if (x.system) {
      x.name.toString
    } else if (x.eres.get._2.isInstanceOf[EnumAst]) {
      outputType(x.eres.get._2.asInstanceOf[EnumAst].param,chareMode)
    } else if (x.eres.get._2.isInstanceOf[EnumInsideAst]) {
      outputType(x.eres.get._2.asInstanceOf[EnumInsideAst].enum.get.param,chareMode)
    } else if (x.eres.get._2.isInstanceOf[EnumInfInst]) {
      outputType(x.eres.get._2.asInstanceOf[EnumInfInst].e.enum.get.param,chareMode)
    } else "Generic"
  }

  def outputType(xx : AstTraversable, x : Generic, chareMode : Boolean) : String = {
    x match {
      case m : Container if chareMode && isProxyType(m.str.scope.last) => {
        val inner = m.lst.head.si.generatePretty
        val ptype = toProxyType(m.str.scope.last)
        s"$ptype$inner" + "_chare_"
      }
      case _ if !xx.eprox.isEmpty => {
        val inner = x.si.generatePretty
        val ptype = ptypeToString(xx.eprox.get)
        s"$ptype$inner" + "_chare_"
      }
      case m : Bound => {
        if (isBasic(m.str)) {
          basicOut(m.str)
        } else if (m.t1.eres != None) {
          outputType(m.t1, chareMode)
        } else "Generic"
      }
      case _ => "Generic"
    }
  }

  def outputChareClassName(m : ClassAst) : String = {
    val scope = m.escope.get.expandName :+ m.ident.name.lst.head
    scope.generatePretty + "_chare_"
  }

  val tokens : HashMap[String, String] = HashMap(
    "#" -> "_POUND_",
    "$" -> "_DOLLAR_",
    "^" -> "_CARET_",
    "!" -> "_BANG_",
    "%" -> "_PERCENT_",
    "=" -> "_EQUAL_",
    "?" -> "_QUESTION_",
    "+" -> "_PLUS_",
    "-" -> "_MINUS_",
    "/" -> "_DIV_",
    "*" -> "_STAR_",
    "[" -> "_LBRACK_",
    "]" -> "_RBRACK_",
    "<" -> "_LT_",
    ">" -> "_GT_",
    "~" -> "_DESTRUCTOR_"
  )

  val assignables : List[String] = List("[]")

  def sanitize(str : String) : String = {
    var nstr : String = str
    for ((x,y) <- tokens) {
      nstr = nstr.replace(x, y)
    }
    nstr
  }

  def outputName(x : String) : String = "_g_" + sanitize(x)
  def outputDefName(x : String) : String = "_def_" + sanitize(x)
  def outputDefNameChare(x : String) : String = "_def_" + sanitize(x) + "_chare_"
  def outputDeclName(x : String) : String = "_decl_" + sanitize(x)
  def outputDeclNameIn(x : String) : String = "_decl_" + sanitize(x) + "_in"

  def generateDefSigMeta(markVirtual : Boolean, m : DefMeta, qual : String, semi : Boolean) {
    val scope = ScopeIdent(List()) :+ m.name.lst.head
    val genName = outputDefName(scope.generatePretty)
    if (verbose) {
      println("generateOuterFunction(META): found defmeta inside class: " + m)
      println("generateOuterFunction(META): name: " + scope)
    }
    val paramsType = m.edef.inherited.getOrElse(m.edef).params.map(x => outputType(x.param, false))
    val paramsName = m.params.zipWithIndex.map(x => "_x" + x._2.toString())
    val paramsAll = (paramsType zip paramsName).map{x => x._1 + " " + x._2}
    val retOut = outputType(m.edef.inherited.getOrElse(m.edef).ret, false)
    val qualName = if (qual == "") genName else qual + "::" + genName
    declFunction(markVirtual, retOut, qualName, paramsAll, semi, false)
  }

  def outputTemplateOrType(x : Type, templateBinds : List[(Generic,Generic)],
                           chareMode : Boolean) : String = {
    if (templateBinds != List())
      outputType(x,Unifier.subst(x.toGeneric,templateBinds),chareMode)
    else
      outputType(x,chareMode)
  }

  def generateDefSig(chareMode : Boolean,
                     insideClass : Boolean, markVirtual : Boolean, m : DefAst,
                     qual : String, semi : Boolean, pure : Boolean,
                     templateBinds : List[(Generic,Generic)] = List(),
                     name : String = "", retOverride : String = "") {
    val scope = (if (!insideClass) m.escope.get.expandName else ScopeIdent(List())) :+ m.ident.name.lst.head
    val genName = if (name == "") outputDefName(scope.generatePretty) else name
    if (verbose) {
      println("generateOuterFunction: found def outside class: " + m)
      println("generateOuterFunction: name: " + scope)
    }
    val paramsType = m.inherited.getOrElse(m).params.map{x =>
      outputTemplateOrType(x.param,templateBinds,chareMode)
    }
    val paramsName = (m.params zip m.inherited.getOrElse(m).params).map(x =>
      if (isBasic(x._1.param.eres.get._1) && !isBasic(x._2.param.eres.get._1))
        outputDeclNameIn(x._1.str)
      else
        outputDeclName(x._1.str)
    )
    var paramsAll = (paramsType zip paramsName).map{x => x._1 + " " + x._2}
    var retOut = outputTemplateOrType(m.inherited.getOrElse(m).ret,templateBinds,chareMode)

    if (m.async) {
      paramsAll ::= "CkFuture f"
      retOut = "void"
    }

    // add in reference if it's an assignable operator
    if (assignables.contains(m.ident.name.lst.head)) {
      retOut = retOut + "&"
    }

    val qualName = if (qual == "") genName else qual + "::" + genName

    declFunction(markVirtual, if (retOverride != "") retOverride else retOut,
                 qualName, paramsAll, semi, pure)
  }

  def generateDeclStub(chareMode : Boolean, insideClass : Boolean, m : DeclAst,
                       templateBinds : List[(Generic,Generic)] = List(), genRhs : Boolean = false) {
    val scope = (if (!insideClass) m.escope.get.expandName else ScopeIdent(List())) :+ m.tparam.str
    val genName = outputDeclName(scope.generatePretty)
    val genType = outputTemplateOrType(m.tparam.param, templateBinds, chareMode)

    val rhs = if (genRhs && !m.expr.isEmpty) {
      m.expr.get.traverse(null, GenState(false, chareMode), generateExpression)
      " = " + m.expr.get.egen
    } else {
      ""
    }

    out +:= genType + " " + genName + rhs + ";"
    m.egen = genName
  }

  def generateNonClsSig(x : AstTraversable, con : AstTraversable) {
    x match {
      // no enclosing class or enclosing def
      case m : DefAst  if m.ecls.isEmpty  && !m.system     => generateDefSig(false, false, false, m, "", true, false)
      case m : DeclAst if m.ecls.isEmpty && m.edef.isEmpty => generateDeclStub(false, false, m)
      case _ => ;
    }
  }

  def generateDefBody(m : DefAst, chareMode : Boolean = false,
                      temp : String = "", tempBinds : List[(Generic,Generic)] = List()) {
    // if this is a constructor generate special setters
    if (m.cons) {
      for (p <- m.params) {
        if (p.set) {
          val declName = outputDeclName(p.str)
          out +:= "this->" + declName + " = " + declName + ";"
        }
      }

      // generate initializers for decls with expressions into the constructor
      for (stmt <- m.ecls.get.slst) {
        stmt match {
          case m : DeclAst if !m.expr.isEmpty => {
            m.expr.get.traverse(null, GenState(false, chareMode), generateExpression)
            val declName = outputDeclName(m.tparam.str)
            val expr = m.expr.get.egen
            if (expr == "NULL") {
              // null ptr
              out +:= "this->" + declName + " = Generic();"
            } else {
              out +:= "this->" + declName + " = " + expr + ";"
            }
          }
          case _ => ;
        }
      }

      // special generation for allocating new Array
      if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array") {
        // assume Arrays are always templated
        out +:= "this->_decl_b = " + " new " + outputType(m,tempBinds.head._2,false) + "[" + "this->_decl_size" + "];"
        out +:= "this->_decl_owner = true;"
      }
      if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array2") {
        // assume Arrays are always templated
        out +:= "this->_decl_b = " + " new " + outputType(m,tempBinds.head._2,false) + "[" + "this->_decl_size1*this->_decl_size2" + "];"
        out +:= "this->_decl_owner = true;"
      }
      if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array3") {
        // assume Arrays are always templated
        out +:= "this->_decl_b = " + " new " + outputType(m,tempBinds.head._2,false) + "[" + "this->_decl_size1*this->_decl_size2*this->_decl_size3" + "];"
        out +:= "this->_decl_owner = true;"
      }
    }

    // special case for array indexing lookup generation
    if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array" && m.ident.name.lst.head == "[]") {
      out +:= """if (_decl_i > this->_decl_size-1) { fprintf(stderr, "array out of bounds access\n"); exit(1); }"""
      out +:= "return this->_decl_b[_decl_i];"
    } else if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array2" && m.ident.name.lst.head == "[]") {
      out +:= "return this->_decl_b[_decl_i*this->_decl_size1 + _decl_j];"
    } else if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array3" && m.ident.name.lst.head == "[]") {
      out +:= "return this->_decl_b[_decl_i*this->_decl_size1*this->_decl_size2 + _decl_j*this->_decl_size2 + _decl_k];"
    }

    if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array2" &&
      (m.ident.name.lst.head == "sliceIRef" || m.ident.name.lst.head == "sliceICopy")) {
      val ttype = outputType(m,tempBinds.head._2,false)
      val isRef : Boolean = m.ident.name.lst.head == "sliceIRef"
      val isRefB = if (isRef) "false" else "true"

      if (temp == "")
        out +:= s"_n__Array* x = new _n__Array();"
      else
        out +:= s"_n__Array_$temp* x = new _n__Array_$temp();"

      out +:= s"x->_decl_owner = $isRefB;"
      out +:= s"x->_decl_size = _decl_j_hi-_decl_j_lo;"

      if (isRef)
        out +:= s"x->_decl_b = this->_decl_b + _decl_i*this->_decl_size1 + _decl_j_lo;"
      else {
        out +:= s"x->_decl_b = new $ttype[x->_decl_size];"
        out +:= """
  for (int i = 0; i < x->_decl_size; i++) {
    x->_decl_b[i] = this->_decl_b[_decl_i*this->_decl_size1 + _decl_j_lo + i];
  }
"""
      }

      out +:= s"return x;"
    }

    if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head == "Array2" && m.ident.name.lst.head == "sliceJCopy") {
      val ttype = outputType(m,tempBinds.head._2,false)

      if (temp == "")
        out +:= s"_n__Array* x = new _n__Array();"
      else
        out +:= s"_n__Array_$temp* x = new _n__Array_$temp();"

      out +:= s"x->_decl_owner = true;"
      out +:= s"x->_decl_size = _decl_i_hi-_decl_i_lo;"

      out +:= s"x->_decl_b = new $ttype[x->_decl_size];"
      out +:= """
  for (int i = 0; i < x->_decl_size; i++) {
    x->_decl_b[i] = this->_decl_b[(_decl_i_lo + i)*this->_decl_size1 + _decl_j];
  }
"""
      out +:= s"return x;"
    }

    if (!m.ecls.isEmpty && m.ecls.get.ident.name.lst.head.startsWith("MSA") && m.ident.name.lst.head.startsWith("MKMSA")) {
      val ttype = outputType(m,tempBinds.head._2,false)
      val nd = m.ident.name.lst.head.substring(5)
      val sizes = nd match {
        case "1D" => "_decl_size"
        case "2D" => "_decl_size1, _decl_size2"
        case "3D" => "_decl_size1, _decl_size2, _decl_size3"
        case  _   => throw new RuntimeException(s"Unsupported MSA Dimensionality, $nd.")
      }

      out +:= s"this->_decl_arr.~MSA$nd();"
      out +:= s"new (&this->_decl_arr) MSA::MSA$nd<$ttype, DefaultEntry<$ttype>, MSA_DEFAULT_ENTRIES_PER_PAGE>($sizes, _decl_numWorkers, _decl_maxBytes);"
    }

    for (stmt <- m.slst) {
      generateInsideStmt(stmt, chareMode)
    }
  }

  def generateAssignment(assignType : String, name : String, expr : String, assign : Boolean) {
    if (assign) {
      out +:= assignType + "& " + name + " = " + expr +  ";"
    } else {
      out +:= assignType + " " + name + " = " + expr +  ";"
    }
  }

  def generateFuncParams(m : FuncCall, defParams : List[TypeParamAst]) : String = {
    var paramTyped = (m.params zip defParams).map{
      x => {
        val paramType = x._1.etype.get._1
        val funType = x._2.param.eres.get._1
        val boundType = outputTypeInner(paramType)
        if (verbose) {
          println("DEFAST: paramType = " + paramType + ", boundType = " + boundType + ", funType = " + funType)
        }
        if (isBasic(funType)) {
          x._1.egen
        } else {
          // if basic then up-convert to Generic, else it is already a generic
          if (isBasic(paramType))
            "Generic((" + boundType + ")" + x._1.egen + ")"
          else
            x._1.egen
        }
      }
    }
    if (FutureConversion.callsAsync(m)) {
      paramTyped +:= m.params.last.egen
    }
    val genParams = if (paramTyped.size == 0) "" else paramTyped.reduceLeft[String](_ + "," + _)
    genParams
  }

  def generateExpression(x : AstTraversable, con : GenState) {
    x match {
      case m : Identifier if !m.partOfType && !m.partOfFun && !m.ered => {
        val name = m.lst.head

        if (verbose) {
          println("generateExpression (identifier): " + m + ", eprev = " + m.eprev + ", etype = " + m.eres.get)
        }

        if (!m.eprev.isEmpty) {
          if (m.eprev.get.egen == "thisIndex") {
            m.egen = m.eprev.get.egen + "." + name
          } else {
            val si = m.etype.get._1
            val t = m.etype.get._2
            val genName = createName
            val prevName = m.eprev.get.egen
            val prevType = testTemplateType(m, verbose = verbose)

            val getField = "((" + prevType._2 + "*)(" + prevName + ".t.v))->" + outputDeclName(name)

            generateAssignment(outputType(si), genName, getField, con.assign)

            m.egen = genName
          }
        } else {

          val genName = m.eres.get._2 match {
            case a : TypeParamAst if a.ecls == None && a.edef == None => {
              val scope = a.escope.get.expandName :+ a.str
              outputDeclName(scope.generatePretty)
            }
            case a : TypeParamAst  => outputDeclName(name)
            case a : EnumInsideAst => {
              if (!a.expr.isEmpty) {
                a.expr.get.traverse(null, con, generateExpression)
                a.expr.get.egen
              } else {
                ""
              }
            }
            case a : EnumInfInst => a.inst
            case _                 => outputName(name)
          }

          m.egen = genName
        }
      }

      // special comparison operator
      case m : FuncCall if (m.t.name.lst.head == "==" || m.t.name.lst.head == "!=")
        && !m.eprev.isEmpty && m.params.size == 1 => {
          val prevName = m.eprev.get.egen
          val genName = createName
          val si = m.etype.get._1
          val retType = outputType(si)
          val retExpr = if (retType == "void") "" else retType + " " + genName + " = "

          val genParams = if (m.params.size == 0) "" else m.params.map(_.egen).reduceLeft[String](_ + "," + _)

          // == OR !=
          val op = m.t.name.lst.head

          // manual handling of NULL for now
          if (prevName == "NULL" || genParams == "NULL") {
            if (prevName == "NULL") {
              out +:= retExpr + prevName + op + genParams + ".t.v" + ";"
            } else if (genParams == "NULL") {
              out +:= retExpr + prevName + ".t.v" + op + genParams + ";"
            }
          } else {
            out +:= retExpr + prevName + op + genParams + ";"
          }

          m.egen = genName
      }

      // a system function call, requires special generation
      case m : FuncCall if m.eres.get._2.isInstanceOf[DefAst] && m.eres.get._2.asInstanceOf[DefAst].system => {
        val systemDef = m.eres.get._2.asInstanceOf[DefAst]
        if (verbose) {
          println("stmt = " + systemDef.lst.get.head)
        }

        // function with $me and possibly $that
        if (m.t.name.lst.head == "println") {
          val paramName = m.params.head.egen
          out +:= "std::cout << " + paramName + " << " + "std::endl;"
        } else if (m.t.name.lst.head == "contribute" && m.params.size == 3) {
          val p = m.params(0)
          m.params match {
            case List(
              _, BinOp(List(id2@Identifier(_)),"."), BinOp(List(id@Identifier(prox), f@FuncCall(Type(Identifier(_),List(),None), _)), "."))
                => {
              val in = p match {
                case BinOp(List(Identifier(lst)),".") => "_decl_" + lst.head
                case BinOp(List(LitType(lit, _)),".") => lit
                case _ => throw new RuntimeException(s"unsupported value $p at ${p.pos}")
              }
              // func call name
              val defast = f.eres.get._2.asInstanceOf[DefAst]
              val scope = ScopeIdent(List()) :+ defast.ident.name.lst.head
              var funcall = outputDefName(scope.generatePretty)

              // proxy name
              val decl = outputDeclName(prox.head)

              val (red, ttype) = id2.eres match {
                case Some((_, fn@DefAst(_,_,_,_,_,_))) if fn.reducer.isDefined => {
                  val scope = fn.escope.get.expandName :+ fn.ident.name.lst.head
                  (s"_reducer${outputDefName(scope.generatePretty)}", outputType(fn.reducer.get.params.head, false))
                }
                case Some((_,eia@EnumInsideAst(Type(Identifier(id::_),_,_),_))) => {
                  eia.enum match {
                    case Some(EnumAst(Type(Identifier(List("Reduction")),_,_),_,_)) =>
                    case _ => throw new RuntimeException(s"unsupported reducer $id2 at ${id2.pos}")
                  }
                  id match {
                    // TODO add other built-ins
                    case "Logical_AND" => ("CkReduction::logical_and_bool", "bool")
                    case "Logical_OR" => ("CkReduction::logical_or_bool", "bool")
                    case _ => throw new RuntimeException(s"unsupported reducer $id2 at ${id2.pos}")
                  }
                }
                case _ => throw new RuntimeException(s"unsupported reducer $id2 at ${id2.pos}")
              }

              // TODO add uid so multiple contributes don't redeclare "result"
              // proxy underlying type
              val proxtype = id.etype.get._1.generatePretty + "_chare_"
              out +:= s"$ttype result = $in;"
              out +:= s"contribute(sizeof($ttype), &result, $red, CkCallback(CkReductionTarget($proxtype, $funcall), $decl));"
            }
            case _ => ;
          }
        } else if (m.t.name.lst.head == "contribute" && m.params.size == 1) {
          val p = m.params(0)
          p match {
            case BinOp(List(id@Identifier(prox), f@FuncCall(Type(Identifier(_),List(),None), List())), ".") => {
              // func call name
              val defast = f.eres.get._2.asInstanceOf[DefAst]
              val scope = ScopeIdent(List()) :+ defast.ident.name.lst.head
              var funcall = outputDefName(scope.generatePretty)

              // proxy name
              val decl = outputDeclName(prox.head)

              // proxy underlying type
              val proxtype = id.etype.get._1.generatePretty + "_chare_"

              out +:= s"contribute(CkCallback(CkReductionTarget($proxtype, $funcall), $decl));"
            }
            case _ => ;
          }
        } else {
          systemDef.lst.get.head match {
            case BinOp(List(LitType(str,_)),".") => {
              if (verbose) println(s"\t str = $str")

              val prevName = if (m.eprev.isEmpty) "" else m.eprev.get.egen
              val nameGenMap  = systemDef.params.map(_.str) zip m.params.map(_.egen)
              var nstr : String = str.replace("$me", prevName)

              for ((name,gen) <- nameGenMap) {
                nstr = nstr.replace("$" + name, gen)
              }

              val si = m.etype.get._1
              val retType = if (nstr.startsWith("_decl_arr.getInitial")) {
                "auto"
              } else {
                outputType(m, m.etype.get._2, con.chareMode)
              }
              val genName = createName
              val retExpr = if (retType == "void") "" else retType + " " + genName + " = "

              if (verbose) println(s"\t nstr = $nstr")

              if (m.t.name.lst.head == "my_pe" && !con.chareMode) {
                out +:= retExpr + s"-1;"
              } else if (m.t.name.lst.head == "pexit" && !con.chareMode) {
                out +:= retExpr + s"exit(0);"
              } else {
                out +:= retExpr + s"($nstr);"
              }

              m.egen = genName
            }
          }
        }
      }

      // generate regular function call if it is not a constructor
      case m : FuncCall if !m.skipGen && !m.ered => {
        if (verbose) {
          println("generateExpression (funccall) (chareMode=" + con.chareMode + "): " +
                  m + ", etype = " + m.etype.get + ", eres = " + m.eres.get)
        }

        val hasPrev = !m.eprev.isEmpty
        val genName = createName

        val pardefAst = m.eres.get._2.asInstanceOf[DefAst]
        val defAst = pardefAst.inherited.getOrElse(pardefAst)
        val defParams = defAst.params

        if (verbose) {
          println("DEFAST = " + defAst + ", params = " + defAst.params)
        }

        // use scope for def name when it is not in a class
        val scope = (if (defAst.ecls.isEmpty) defAst.escope.get.expandName else ScopeIdent(List())) :+
                      defAst.ident.name.lst.head
        var funCallName = outputDefName(scope.generatePretty)

        if (m.superCall) {
          val parent = m.ecls.get.parent.get
          val cls = parent.eres.get._2.asInstanceOf[ClassAst]
          funCallName = outputClassName(cls) + "::" + funCallName
        }

        val genParams = generateFuncParams(m, defParams)

        var prevExpr = ""
        if (hasPrev) {
          val prevName = m.eprev.get.egen
          val prevType = testTemplateType(m, defAst.cons, verbose = verbose)

          // is it a proxy type?
          if ((prevType._3 || !m.eprev.get.eprox.isEmpty) && con.chareMode) {
            prevExpr = s"$prevName."
          } else {
            val tname = prevType._2
            prevExpr = s"(($tname*)($prevName.t.v))->"
          }
        }

        // TODO check difference between isTempBasic vs. isBasic
        val needsConvert = pardefAst.ret.eres match {
          case Some((_,n@ClassGeneric(_,t@_,_))) =>
            ((n.templ || t.istempl) && isTempBasic(m.etype.get._1))
          case _ => false
        }
        // if constructor then void return
        val retType = 
          if (defAst.cons || defAst.async) "void"
          else if (needsConvert) basicOut(m.etype.get._1) 
          else outputType(pardefAst.ret,con.chareMode)
        val assign = if (con.assign) "&" else ""
        val retExpr = if (retType == "void") "" else retType + assign + " " + genName + " = "
        val definedRet = defAst.ret.eres.get._1
        val retConvert =
          if (retType == "void") ""
          else if (needsConvert && isBasic(m.etype.get._1)) s".t.${genOut(m.etype.get._1)}"
          else {
          pardefAst.ret.eres.get._2 match {
            case n : ClassGeneric if n.templ && isTempBasic(m.etype.get._1) => ""
            case _ => ""
          }
        }

        out +:= retExpr + prevExpr + funCallName + "(" + genParams + ")" + retConvert + ";"

        m.egen = genName
      }

      case m : LitType if !m.ered => {
        if (m.string) {
          m.egen = "std::string(\"" + m.lit + "\")"
        } else if (m.lit == "thisProxy") {
          m.egen = if (con.chareMode) "thisProxy" else "Generic(this)"
        } else {
          m.egen = m.lit
        }
      }

      case m : BinOp => {
        m.egen = m.lhs.last.egen
        if (verbose) println("generateExpression (binop): " + m + ", egen = " + m.egen)
      }

      case m : DeleteAst => {
        if (verbose) println("generateInsideStmt: DeleteAst: " + m)
        val prevType = m.expr.etype.get._1.generatePretty
        out +:= "delete (" + prevType + "*)" + m.expr.egen + ".t.v" + ";"
      }

      case m : NewAst => {
        if (verbose) println("generateExpression (newast) chareMode=" + con.chareMode + ": " + m)

        m.expr match {
          case f : FuncCall => {
            val genName = createName
            var isTemplate : Option[ClassAst] = None
            var clsFound : Option[ClassAst] = None
            var generatedChare = false

            f.eres.get._2 match {
              case n : DefAst => {
                n.oldRet.eres.get._2 match {
                  case o : ClassAst => {
                    clsFound = Some(o)
                    if (o.istemplate) isTemplate = Some(o)
                  }
                  case _ => ;
                }
              }
              case _ => ;
            }

            if (verbose) {
              println("NewAst (" + m + "): " + clsFound + ", array = " + clsFound.get.charearray)
            }

            if (isTemplate.isEmpty) {
              if (con.chareMode && m.isProxyNew) {
                val clsName = f.etype.get._1.generatePretty + "_chare_"
                val genParams = generateFuncParams(f, f.eres.get._2.asInstanceOf[DefAst].params)
                if (!clsFound.get.charearray.isEmpty) {
                  if (m.proxyExpr.size != clsFound.get.charearray.get.dim) {
                    SemanticError("proxy array allocation dimensions must match definition", m.pos)
                  }
                  for (e <- m.proxyExpr) generateExpression(e, con)
                  val names = if (m.proxyExpr.size == 0) ""
                              else "," + m.proxyExpr.map(_.egen).reduceLeft(_ + "," + _)
                  out +:= s"CProxy_$clsName $genName = CProxy_$clsName::ckNew($genParams$names);"
                } else {
                  out +:= s"CProxy_$clsName $genName = CProxy_$clsName::ckNew($genParams);"
                }
                generatedChare = true
              } else {
                val clsName = f.etype.get._1.generatePretty

                if (verbose) println("*** NewAst: " + m + ", cls = " + clsName)

                out +:= s"Generic $genName = Generic((void*)(new $clsName()));"
              }
            } else {
              var clsName = ""
              f.etype.get._2 match {
                // there must be exactly one generic in template type, thus Container
                case n : Container => {
                  if (isTempBasic(n.lst.head.si)) {
                    clsName = outputClassName(isTemplate.get, tempBasicOut(n.lst.head.si))
                  } else {
                    clsName = outputClassName(isTemplate.get, "")
                  }
                }
              }
              // set the template class for later
              m.templateCls = isTemplate

              out +:= s"Generic $genName = Generic((void*)(new $clsName()));"
            }

            // set previous name for the function generation
            m.egen = genName

            if (!generatedChare) {
              // now generate the payload with this NewAst as the previous
              f.eprev = Some(m)

              // generate function for 'this' constructor if not a chare
              val prevSkip = f.skipGen
              f.skipGen = false
              generateExpression(f, con);
              f.skipGen = prevSkip
            } else {
              f.skipGen = true
            }
          }
          case _ => ;
        }
      }

      case m : InternalNode => {
        m.egen = m.raw
      }

      case m : InternalGroup => {
        m.egen = m.raw.map(_.egen) mkString ""
      }

      case _ => ;
    }
  }

  def generateInsideStmt(x : AstTraversable, chareMode : Boolean) {
    x match {
      case m : DeclAst => {
        generateDeclStub(chareMode, true, m, genRhs = !m.expr.isEmpty)
      }

      case m : AssignAst => {
        m.expr.traverse(null, GenState(true, chareMode), generateExpression)
        m.expr2.traverse(null, GenState(false, chareMode), generateExpression)
        val expr = m.expr2.egen
        if (expr == "NULL") {
          // null ptr
          out +:= m.expr.egen + " = Generic();"
        } else {
          out +:= m.expr.egen + " = " + m.expr2.egen + ";"
        }
      }

      case m : BinOp   => {
        // post order traversal on the expression for generation
        if (verbose) {
          println("generateInsideStmt: binop = " + m)
        }
        m.traverse(null, GenState(false, chareMode), generateExpression)
      }

      case m : FuncCall => {
        m.traverse(null, GenState(false, chareMode), generateExpression)
      }

      case m : ReturnAst => {
        val rtrn = if (reducerCon.isEmpty) "return" else reducerCon.get + " ="

        out +:= ((m.expr) match {
          case Some(e) => {
            // TODO: fix this
            //val assignable = assignables.contains(m.edef.get.ident.name.lst.head)
            e.traverse(null, GenState(false, chareMode), generateExpression)
            // null ptr
            rtrn + " " + (if (e.egen != "NULL") e.egen.toString else "Generic()") + ";"
          }
          case None => rtrn + ";"
        })

        if (!reducerCon.isEmpty) {
          out +:= "continue;"
        }
      }

      case m : WhenAst => {
        out +:= "{"
        out>>1

        out +:= s"/* ${m.getSpecificName} */"
        m.slst.foreach(generateInsideStmt(_, chareMode))

        out<<1
        out +:= "}"
      }

      case m : IfAst => {
        m.expr.traverse(null, GenState(false, chareMode), generateExpression)
        out +:= "if (" + m.expr.egen + ")"
        out +:= "{"
        out>>1
        generateInsideStmt(m.then, chareMode)
        out<<1
        out +:= "}"
        if (!m.el.isEmpty) {
          out +:= "else"
          out +:= "{"
          out>>1
          generateInsideStmt(m.el.get, chareMode)
          out<<1
          out +:= "}"
        }
      }

      case m : WhileAst => {
        m.cond.traverse(null, GenState(false, chareMode), generateExpression)
        out +:= "while (" + m.cond.egen + ")"
        val condVar = m.cond.egen
        out +:= "{"
        out>>1
        generateInsideStmt(m.body, chareMode)
        m.cond.traverse(null, GenState(false, chareMode), generateExpression)
        out +:= condVar + " = " + m.cond.egen + ";"
        out<<1
        out +:= "}"
      }

      case m : DoWhileAst => {
        val condVar = createName
        out +:= "bool " + condVar + ";"
        out +:= "do"
        out +:= "{"
        out>>1
        generateInsideStmt(m.body, chareMode)
        m.cond.traverse(null, GenState(false, chareMode), generateExpression)
        out +:= condVar + "=" + m.cond.egen + ";"
        out<<1
        out +:= "} while (" + condVar + ");"
      }

      case m : ForAst => {
        // already wrapped in namespace, hence no additonal wrapping for decls
        for (d <- m.decls)
          generateInsideStmt(d, chareMode)
        m.expr1.foreach(x => x.traverse(null, GenState(false, chareMode), generateExpression))
        val cond = m.expr1.map(_.egen).reduceLeft[String](_ + "&&" + _)
        val condVar = createName
        out +:= "bool " + condVar + " = " + cond + ";"
        out +:= "while (" + condVar + ")"
        out +:= "{"
        out>>1

        // body of while
        generateInsideStmt(m.body, chareMode)

        // generate updates
        m.expr2.foreach(x => x.traverse(null, GenState(false, chareMode), generateExpression))

        // generate new condition to check
        m.expr1.foreach(x => x.traverse(null, GenState(false, chareMode), generateExpression))
        val cond2 = m.expr1.map(_.egen).reduceLeft[String](_ + "&&" + _)
        out +:= condVar + " = " + cond2 + ";"

        out<<1
        out +:= "}"
      }

      case m : Namespace => {
        out +:= "{"
        out>>1
        for (i <- m.slst) {
          generateInsideStmt(i, chareMode)
        }
        out<<1
        out +:= "}"
      }

      case InternalNode(raw) => {
        out +:= raw
      }

      case m : InternalGroup => {
        m.raw.map(generateInsideStmt(_, chareMode))
      }

      case _ => ;
    }
  }

  // TODO : this will fail for inline functions, make it a param like chareMode!
  var reducerCon : Option[String] = None

  def generateNonClsDefBody(x : AstTraversable, con : AstTraversable) {
    x match {
      // handle unenclosed reducers
      case m : DefAst if m.ecls.isEmpty && !m.system && !m.reducer.isEmpty => {
        val scope = m.escope.get.expandName :+ m.ident.name.lst.head
        val name  = outputDefName(scope.generatePretty)
        val ttype = outputType(m.reducer.get.params.head, false)
        val pname = (m.params zip m.inherited.getOrElse(m).params).map(x =>
          if (isBasic(x._1.param.eres.get._1) && !isBasic(x._2.param.eres.get._1))
            outputDeclNameIn(x._1.str)
          else
            outputDeclName(x._1.str)
        )
        val accum = pname(0)
        val x = pname(1)
        out +:= "CkReductionMsg* " + name + "(int nMsg, CkReductionMsg** msgs) {"
        out>>1
        out +:= ttype + " " + accum + ";"
        out +:= "if (nMsg >= 1) {"
        out>>1
        out +:= "PUP::fromMem p(msgs[0]->getData());"
        out +:= s"p | $accum;"
        out<<1
        out +:= "}"
        out +:= "for (int i = 1; i <= (nMsg - 1); i += 1) {"
        out>>1
        out +:= ttype + " " + x + ";"
        out +:= "PUP::fromMem p(msgs[i]->getData());"
        out +:= "p | " + x + ";";
        reducerCon = Some(accum)
        generateDefBody(m)
        reducerCon = None
        out<<1
        out +:= "}"
        out +:= "// copy the accumulator to the heap so it lives on"
        out +:= ttype + " *toSend = new " + ttype + "(" + accum + ");"
        out +:= "return CkReductionMsg::buildNew(sizeof(" + ttype + "), toSend);"
        out<<1
        out +:= "}"
        out +:= "/*global*/ CkReduction::reducerType _reducer" + name + ";"
        out +:= "/*initnode*/ void _register" + name + "(void) {"
        out>>1
        out +:= "_reducer" + name + " = CkReduction::addReducer(" + name + ");"
        out<<1
        out +:= "}"
        registerInitCalls += "_register" + name
      }
      // no enclosing class or enclosing def
      case m : DefAst  if m.ecls.isEmpty && !m.system => {
        generateDefSig(false, false, false, m, "", false, false)
        out +:= "{"
        out>>1
        generateDefBody(m)
        out<<1
        out +:= "}"
      }
      case _ => ;
    }
  }

  def generateClassDecl(m : ClassAst, temp : String = "") {
    val inheritLst : ListBuffer[String] = ListBuffer()

    if (!m.parent.isEmpty) {
      val parent = m.parent.get
      val cls = parent.eres.get._2.asInstanceOf[ClassAst]
      inheritLst += " public " + outputClassName(cls)
    }

    inheritLst ++= m.traits.map(x => " public " + outputClassName(x.eres.get._2.asInstanceOf[ClassAst]))

    var inheritance : String = if (inheritLst.size == 0) "" else ":" + inheritLst.reduceLeft[String](_ + "," + _)
    val colon = if (inheritLst.size == 0) ":" else ","

    if ((inheritLst.size == 0 || m.parent.isEmpty) && !m.istrait) {
      inheritance = inheritance + s"""
  #if defined(__PARALLEL__)
  $colon public PUP::able
  #endif
"""
    }

    if (verbose) {
      println("generateClassDecl: " + m + ", inheritMethods = " + m.inheritMethods + ", methods = " + m.thisMethods);
    }

    val tempBinds = if (m.istemplate) List((m.ident.lst.head.toGeneric, Bound(ScopeIdent(List(SystemIdentifier.namespace,temp)), m.ident.lst.head))) else List()

    if (verbose) {
      println("generateClassDecl: " + m + ", template = " + temp + ", binds = " + tempBinds);
    }

    val clsName = outputClassName(m, temp)

    out +:= s"struct $clsName $inheritance"
    out +:= "{"
    out>>1
    if (!m.abs) {
      // generate default constructor if the class is not abstract
      out +:= s"$clsName(){}"
    }

    var hasDestructor : Boolean = false

    for (stmt <- m.slst) {
      stmt match {
        case n : DefAst if !n.system  => {
          val retOverride : String = if (m.ident.name.lst.head.startsWith("MSA") && (n.ident.name.lst.head == "getInitialWrite" || n.ident.name.lst.head == "getInitialAccum")) {
            val ttype = outputType(m,tempBinds.head._2,false)
            s"MSA::MSA${m.ident.name.lst.head.substring(3)}D<$ttype, DefaultEntry<$ttype>, MSA_DEFAULT_ENTRIES_PER_PAGE>::${n.ident.name.lst.head.substring(10)}"
          } else {
            ""
          }
          generateDefSig(false, true, true, n, "", true, n.abs, tempBinds, retOverride = retOverride)
          if (n.ident.name.lst.head == "~this")
            hasDestructor = true
        }
        case n : DeclAst              => {
          // special case generation of "b" parameter for arrays
          if ((m.ident.name.lst.head == "Array"  ||
               m.ident.name.lst.head == "Array2" ||
               m.ident.name.lst.head == "Array3") && n.tparam.str == "b") {
            out +:= outputType(m,tempBinds.head._2,false) + "* _decl_b;"
          } else if (m.ident.name.lst.head.startsWith("MSA") && n.tparam.str == "arr") {
            val ttype = outputType(m,tempBinds.head._2,false)
            out +:= s"MSA::MSA${m.ident.name.lst.head.substring(3)}D<$ttype, DefaultEntry<$ttype>, MSA_DEFAULT_ENTRIES_PER_PAGE> _decl_arr;";
          } else if (m.ident.name.lst.head.startsWith("Future") && n.tparam.str == "f") {
            out +:= s"CkFuture _decl_f;";
            out +:= s"""static _n__Future* wrapCkFuture(CkFuture f) { _n__Future *f_ = new _n__Future(); f_->_decl_f = f; return f_; }"""
          } else {
            generateDeclStub(false, true, n, tempBinds)
          }
        }
        case _ => ;
      }
    }

    if (m.ident.name.lst.head == "Array" || m.ident.name.lst.head == "Array2" || m.ident.name.lst.head == "Array3") {
      out +:= s"virtual ~$clsName() { if (_decl_owner) delete [] _decl_b; }"
    } else {
      // generate virutal destructor regardless of abstractness of class
      out +:= s"virtual ~$clsName() {"
      if (hasDestructor) {
        out>>1
        out +:= "_DESTRUCTOR_this();"
        out<<1
      }
      out +:= "}"
    }

    for (meta <- m.inheritMethods -- m.thisMethods) {
      val matching = if (m.thisMethods.size == 0) false else m.thisMethods.map(meta.compare(_)).reduceLeft[Boolean](_ || _)
      if (!matching) {
        if (verbose) {
          println(m + ": META: " + m + ": def meta = " + meta + ", enclosing = " + meta.ecls)
        }
        generateDefSigMeta(true, meta, "", true)
      }
    }

    val clsname = outputClassName(m, temp)

    if (!m.istrait) {

      out +:= "#if defined(__PARALLEL__)"
      if (m.abs) {
        out +:= s"PUPable_abstract($clsname);"
      } else {
        out +:= s"PUPable_decl($clsname);"
      }
      out +:= "#endif"

       if (!m.abs) {
         out +:= "#if defined(__PARALLEL__)"
         out +:= s"$clsname(CkMigrateMessage*) {}"
         out +:= "#endif"
       }

       out +:= "#if defined(__PARALLEL__)"
       out +:= "void pup(PUP::er &p)"
       out +:= "{"
       out>>1
       out +:= "pack(p);"
       if (!m.parent.isEmpty) {
         val parent = m.parent.get
         val cls = parent.eres.get._2.asInstanceOf[ClassAst]
         val parentCls = outputClassName(cls)
         out +:= s"$parentCls::pack(p);"
       }
       out<<1
       out +:= "}"
       out +:= "#endif"
     }

    out +:= "#if defined(__PARALLEL__)"
    out +:= "void pack(PUP::er &p);"
    out +:= "#endif"

    out<<1
    out +:= "};"
  }

  def findClassDecl(x : AstTraversable, lst : ListBuffer[ClassAst]) {
    x match {
      case m : ClassAst if !isBasicDecl(m.ident.name.lst.head) && !m.chare => lst += m
      case _ => ;
    }
  }

  def generateDeclInit(x : AstTraversable, con : AstTraversable) {
    x match {
      case m : DeclAst if m.ecls == None && m.edef == None && m.expr != None => {
        val scope = m.escope.get.expandName :+ m.tparam.str
        val declName = outputDeclName(scope.generatePretty)
        m.expr.get.traverse(null, GenState(true, false), generateExpression)
        val result = m.expr.get.egen
        out +:= s"$declName = $result;"
      }
      case _ => ;
    }
  }

  def generateClsDefBody(x : AstTraversable, con : AstTraversable) {
    x match {
      case m : ClassAst if !isBasicDecl(m.ident.name.lst.head) && !m.chare => {

        // generate inner class
        generateClassInner()

        // if it is a template generate specializations
        if (m.istemplate) {
          templateGenSet.map{x => generateClassInner(x._1) }
        }

        def generateClassInner(temp : String = "") {
          val clsName = outputClassName(m, temp)

          val tempBinds = if (m.istemplate) List((m.ident.lst.head.toGeneric, Bound(ScopeIdent(List(SystemIdentifier.namespace,temp)), m.ident.lst.head))) else List()

          for (stmt <- m.slst) {
            stmt match {
              case n : DefAst if !n.abs && !n.system => {
                val retOverride : String = if (m.ident.name.lst.head.startsWith("MSA") && (n.ident.name.lst.head == "getInitialWrite" || n.ident.name.lst.head == "getInitialAccum")) {
                  val ttype = outputType(m,tempBinds.head._2,false)
                  s"MSA::MSA${m.ident.name.lst.head.substring(3)}D<$ttype, DefaultEntry<$ttype>, MSA_DEFAULT_ENTRIES_PER_PAGE>::${n.ident.name.lst.head.substring(10)}"
                } else {
                  ""
                }
                generateDefSig(false, true, false, n, clsName, false, false, tempBinds, retOverride = retOverride)
                out +:= "{"
                out>>1
                for (p <- n.params zip n.inherited.getOrElse(n).params) {
                  if (isBasic(p._1.param.eres.get._1) && !isBasic(p._2.param.eres.get._1)) {
                    out +:= outputType(p._1.param,false) + " " + outputDeclName(p._1.str) + " = " + outputDeclNameIn(p._1.str) + ".t." + genOut(p._1.param.eres.get._1) + ";"
                  }
                }

                generateDefBody(n, false, temp, tempBinds)
                out<<1
                out +:= "}"
              }
              case _ => ;
            }
          }

          if (!m.istrait && !m.abs) {
            val clsName = outputClassName(m, temp)

            out +:= "#if defined(__PARALLEL__)"
            out +:= s"PUPable_def($clsName);"
            out +:= "#endif"

            registerPUPables += clsName
          }

          out +:= "#if defined(__PARALLEL__)"
          out +:= s"void $clsName::pack(PUP::er &p)"
          out +:= "{"
          out>>1
          for (stmt <- m.slst) {
            stmt match {
              case n : DeclAst => {
                if (m.ident.name.lst.head == "Array" && n.tparam.str == "b") {
                  val spec = outputType(m,tempBinds.head._2,false)
                  out +:= "if (p.isUnpacking())"
                  out +:= "{"
                  out>>1
                  out +:= s"_decl_b = new $spec[_decl_size];"
                  out<<1
                  out +:= "}"

                  out +:= "PUParray(p, _decl_b, _decl_size);"
                } else if (m.ident.name.lst.head == "Array2" && n.tparam.str == "b") {
                  val spec = outputType(m,tempBinds.head._2,false)
                  out +:= "if (p.isUnpacking())"
                  out +:= "{"
                  out>>1
                  out +:= s"_decl_b = new $spec[_decl_size1*_decl_size2];"
                  out<<1
                  out +:= "}"

                  out +:= "PUParray(p, _decl_b, _decl_size1*_decl_size2);"
                } else if (m.ident.name.lst.head == "Array3" && n.tparam.str == "b") {
                  val spec = outputType(m,tempBinds.head._2,false)
                  out +:= "if (p.isUnpacking())"
                  out +:= "{"
                  out>>1
                  out +:= s"_decl_b = new $spec[_decl_size1*_decl_size2*_decl_size3];"
                  out<<1
                  out +:= "}"

                  out +:= "PUParray(p, _decl_b, _decl_size1*_decl_size2*_decl_size3);"
                } else {
                  val declname = outputDeclName(n.tparam.str)
                  out +:= s"p | $declname;"
                }
              }
              case _ => ;
            }
          }
          out<<1
          out +:= "}"
          out +:= "#endif"

          for (meta <- m.inheritMethods -- m.thisMethods) {
            val matching = if (m.thisMethods.size == 0) false else m.thisMethods.map(meta.compare(_)).reduceLeft[Boolean](_ || _)
            if (!matching) {
              if (verbose) {
                println("META body: " + m + ": def meta = " + meta + ", enclosing = " + meta.ecls)
              }
              generateDefSigMeta(false, meta, clsName, false)
              out +:= "{"
              out>>1
              val scope = ScopeIdent(List()) :+ meta.name.lst.head
              val metaName = outputDefName(scope.generatePretty)
              val paramsName = meta.params.zipWithIndex.map(x => "_x" + x._2.toString())
              val params = if (paramsName.size == 0) "" else paramsName.reduceLeft[String](_ + "," + _)
              val retType = outputType(meta.ret.si)
              val ret = if (retType != "void") "return " else ""
              out +:= ret + outputClassName(meta.ecls.get) + "::" + metaName + "(" + params + ")" + ";"
              out<<1
              out +:= "}"
            }
          }
        }
      }
      case _ => ;
    }
  }

  def findChareClass(x : AstTraversable, chareCls : ListBuffer[ClassAst]) : Unit = {
    x match {
      case m : ClassAst if m.chare && !m.abs => chareCls += m
      case _ => ;
    }
  }

  def findCriteriaChare(x : AstTraversable, c : Criteria) : Unit = {
    x match {
      case m : ClassAst if m.chare => {
        if (!m.parent.isEmpty) {
          val clsPar = m.parent.get.eres.get._2.asInstanceOf[ClassAst]
          if (clsPar == c.find) {
            if (m.abs) c.abs += m
            else       c.con += m
          }
        }
        for (t <- m.traits) {
          val clsTrait = t.get.eres.get._2.asInstanceOf[ClassAst]
          if (clsTrait == c.find) {
            if (m.abs) c.abs += m
            else       c.con += m
          }
        }
      }
      case _ => ;
    }
  }

  case class Criteria(var find : ClassAst, var abs : ListBuffer[ClassAst], con : ListBuffer[ClassAst])

  def findAbstractCharesDecl(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst if m.chare && m.abs => {
        val absProxyName = "CProxy_" + outputChareClassName(m)
        out +:= s"struct $absProxyName;"
      }
      case _ => ;
    }
  }

  def findAbstractChares(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst if m.chare && m.abs => {
        if (verbose) {
          println("findAbstractChares: " + m)
        }

        val c : Criteria = Criteria(m, ListBuffer(m), ListBuffer())

        while (c.abs.size > 0) {
          val cur : ListBuffer[ClassAst] = c.abs
          c.abs = ListBuffer()
          for (x <- cur) {
            c.find = x
            tree.traverse(findCriteriaChare, c, null)
            if (verbose) {
              println("abs = " + c.abs + ", con = " + c.con)
            }
          }
        }

        val absProxyName = "CProxy_" + outputChareClassName(m)

        out +:= s"struct $absProxyName {"
        out>>1
        out +:= "int handle;"

        // generate storage for each proxy
        for ((p,i) <- c.con zipWithIndex) {
          val clsName = outputChareClassName(p)
          out +:= s"CProxy_$clsName p$i;"
        }

        // generate pup for each proxy
        out +:= "void pup(PUP::er &p) {"
        out>>1
        out +:= "p | handle;"
        for ((p,i) <- c.con zipWithIndex) {
          out +:= s"p | p$i;"
        }
        out<<1
        out +:= "}"

        // generate constructor for each proxy
        for ((p,i) <- c.con zipWithIndex) {
          val clsName = outputChareClassName(p)
          out +:= s"$absProxyName(CProxy_$clsName x) : handle($i), p$i(x) {};"
        }

        // generate default constructor
        out +:= s"$absProxyName() : handle(-1) {};"

        // generate virtual dispatch calls
        for (stmt <- m.slst) {
          stmt match {
            case n : DefAst => {
              val defName = outputDefName((ScopeIdent(List()) :+ n.ident.name.lst.head).generatePretty)
              val paramsName = n.params.map(x => outputDeclName(x.str))
              val params = if (paramsName.size > 0) paramsName.reduceLeft[String](_ ++ "," ++ _) else ""

              generateDefSig(true, true, false, n, "", false, false, List())
              out +:= "{"
              out>>1
              out +:= "switch (handle) {"
              out>>1
              for ((p,i) <- c.con zipWithIndex) {
                out +:= s"case $i: p$i.$defName($params); break;"
              }
              out +:= "default: assert(0); break;"
              out<<1
              out +:= "}"
              out<<1
              out +:= "}"
            }
            case _ => ;
          }
        }

        out<<1
        out +:= "};"
      }
      case _ => ;
    }
  }

  def outputChareInterfaceMain() {
    outci +:= "{"
    outci>>1
    outci +:= s"""MainChare* c = new MainChare("Main");"""
    outci +:= s"""ConsEntry* cons = new ConsEntry("Main");"""
    outci +:= s"""cons->addEntryParameter(new Parameter(new PtrType(new Type("CkArgMsg")), "m"));"""
    outci +:= s"""c->addEntry(cons);"""
    outci +:= s"""m->addModuleEntity(c);"""
    outci<<1
    outci +:= "}"
  }

  def charmInterfaceEntry(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : DefAst if m.entry && !m.cons => {
        val scope = ScopeIdent(List()) :+ m.ident.name.lst.head
        val genName = outputDefName(scope.generatePretty)
        val ttype = if (m.sync) outputType(m.ret,true) else "void" 
        // return type is always void per the spec
        outci +:= "{"
        outci>>1
        outci +:= s"""Entry* e = new Entry(new Type("$ttype"), "$genName");"""

        // generate parameters
        // @TODO: this should be converted to a message
        for (p <- m.params) {
          val ptype = outputTemplateOrType(p.param, List(), true)
          val pname = outputDeclName(p.str)
          outci +:= s"""e->addEntryParameter(new Parameter(new Type("$ptype"), "$pname"));"""
        }

        if (m.async) {
          outci +:= s"""e->addEntryParameter(new Parameter(new Type("CkFuture"), "f"));"""
        }

        if (m.sync) {
          outci +:= "e->addAttribute(SYNC);"  
        }

        if (m.threaded) {
          outci +:= "e->addAttribute(THREADED);"  
        }

        // sync methods cannot be reduction targets
        if (!m.sync) {
          outci +:= "e->addAttribute(REDUCTIONTARGET);"
        }

        outci +:= s"""c->addEntry(e);"""
        outci<<1
        outci +:= "}"
      }
      case _ => ;
    }
  }

  def outputCharmInterface(x : ClassAst) : Unit = {
    val cls = outputChareClassName(x)
    outci +:= "{"
    outci>>1

    if (x.mainchare) {
      outci +:= s"""MainChare* c = new MainChare("$cls");"""
      outci +:= "{"
      outci>>1
      outci +:= s"""ConsEntry* cons = new ConsEntry("$cls");"""
      outci +:= s"""cons->addEntryParameter(new Parameter(new PtrType(new Type("CkArgMsg")), "m"));"""
      outci +:= s"""c->addEntry(cons);"""
      outci<<1
      outci +:= "}"
    } else if (!x.charearray.isEmpty) {
      val dim = x.charearray.get.dim + "D"
      outci +:= s"""Array* c = new Array("$cls", \"$dim\");"""
    } else {
      outci +:= s"""Chare* c = new Chare("$cls");"""
    }

    var consDef : Option[DefAst] = None
    for (stmt <- x.slst) {
      stmt match {
        case n : DefAst if n.ident.name.lst.head == "this" => consDef = Some(n)
        case _ => ;
      }
    }

    // constructor
    if (!x.mainchare) {
      outci +:= s"""ConsEntry* cons = new ConsEntry("$cls");"""
      if (!consDef.isEmpty) {
        for (p <- consDef.get.params) {
          val ptype = outputTemplateOrType(p.param, List(), true)
          val pname = outputDeclName(p.str)
          outci +:= s"""cons->addEntryParameter(new Parameter(new Type("$ptype"), "$pname"));"""
        }
      }
      outci +:= s"""c->addEntry(cons);"""
    }

    // methods
    outci +:= "{"
    outci>>1
    x.traverse(charmInterfaceEntry, null, null)
    outci<<1
    outci +:= "}"

    outci +:= s"""m->addModuleEntity(c);"""
    outci<<1
    outci +:= "}"
  }

  def outputCharmClassDecl(m : ClassAst) {
    val clsName = outputChareClassName(m)
    out +:= "struct " + clsName + " : public " + "CBase_" + clsName
    out +:= "{"
    out>>1
    var cons : Option[DefAst] = None
    for (stmt <- m.slst) {
      stmt match {
        case n : DefAst  => {
          if (n.ident.name.lst.head == "this") {
            cons = Some(n)
          }
          generateDefSig(true, true, false, n, "", true, n.abs, List())
        }
        case n : DeclAst => generateDeclStub(true, true, n, List())
        case _ => ;
      }
    }

    if (!m.charearray.isEmpty) {
      out +:= s"$clsName(CkMigrateMessage*) {}"
    }

    if (m.mainchare) {
      out +:= clsName + "(CkArgMsg* m);"
    } else {
      // generate default constructor
      if (cons.isEmpty || cons.get.params.size == 0) {
        out +:= clsName + "();"
      } else {
        generateDefSig(true, true, false, cons.get, "", true, cons.get.abs, List(), clsName, " ")
      }
    }

    // add in pupper
    out +:= "void pup(PUP::er &p);"

    out<<1
    out +:= "};"
  }

  def generateChareDefBody(x : AstTraversable, con : AstTraversable) : Unit = {
    x match {
      case m : ClassAst if m.chare && !m.abs => {
        val clsName = outputChareClassName(m)

        for (stmt <- m.slst) {
          stmt match {
            case n : DefAst => {
              if (n.ident.name.lst.head == "this") {
                if (m.mainchare) {
                  out +:= s"$clsName::$clsName(CkArgMsg* m)"
                } else {
                  generateDefSig(true, true, false, n, clsName, false, false, List(), clsName, " ")
                }
                val paramsName = n.params.map(x => outputDeclName(x.str))
                val params = if (paramsName.size > 0) paramsName.reduceLeft[String](_ ++ "," ++ _) else ""
                out +:= "{"
                out>>1
                if (m.mainchare) {
                  if (n.params.size == 0) {
                    out +:= s"_def_this();"
                  } else if (n.params.size == 1) {
                    out +:= """
  Generic MAIN_gen_1 = Generic((void*)(new _n__Array()));
  ((_n__Array*)(MAIN_gen_1.t.v))->_def_this(m->argc);
  for (int i = 0; i < m->argc; i++) {
    ((_n__Array*)(MAIN_gen_1.t.v))->_def__LBRACK__RBRACK_(i) = m->argv[i];
  }
  _def_this(MAIN_gen_1);
"""
                  }
                } else {
                  out +:= s"_def_this($params);"
                }
                out<<1
                out +:= "}"
              }
              generateDefSig(true, true, false, n, clsName, false, false, List())
              out +:= "{"
              out>>1
              generateDefBody(n, true)
              out<<1
              out +:= "}"
            }
            case _ => ;
          }
        }

        out +:= s"void $clsName::pup(PUP::er &p)"
        out +:= "{"
        out>>1
        for (stmt <- m.slst) {
          stmt match {
            case n : DeclAst => {
              val declName = outputDeclName(n.tparam.str)
              out +:= s"p | $declName;"
            }
            case _ => ;
          }
        }
        out<<1
        out +:= "}"
      }
      case _ => ;
    }
  }

  def generateNonClsChareSig(x : AstTraversable, con : AstTraversable) {
    x match {
      case m : DefAst  if m.ecls.isEmpty && !m.system => {
        val str = (m.escope.get.expandName :+ m.ident.name.lst.head).generatePretty
        val defName = outputDefNameChare(str)
        generateDefSig(true, false, false, m, "", true, false, List(), defName)
      }
      case _ => ;
    }
  }

  def generateNonClsChare(x : AstTraversable, con : AstTraversable) {
    x match {
      case m : DefAst  if m.ecls.isEmpty && !m.system => {
        val str = (m.escope.get.expandName :+ m.ident.name.lst.head).generatePretty
        val defName = outputDefNameChare(str)
        generateDefSig(true, false, false, m, "", false, false, List(), defName)
        out +:= "{"
        out>>1
        generateDefBody(m, true)
        out<<1
        out +:= "}"
      }
      case _ => ;
    }
  }

}
