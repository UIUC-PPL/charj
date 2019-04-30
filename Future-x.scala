package FrontEnd

object FutureConversion {
  import SystemIdentifier.namespace

  def callsAsync(fc : FuncCall) : Boolean =
    fc.eres match {
      case Some((_,f@DefAst(_,_,_,_,_,_))) => f.async
      case _ => false
    }
  
  def callsFuture(t : Type) : Boolean =
    t.eres match {
      case Some((ScopeIdent(List(`namespace`,"_FUTURE_",_)),_)) => true
      case _ => false
    }

  def consFuture(n : NewAst) : Boolean =
    n.expr match {
      case FuncCall(Type(Identifier(List("Future")),_,_),List()) => true
      case _ => false
    }
}

class FutureConversion(tree : AstTraversable, verbose : Boolean) {
  import ModifyASTReplace._
  import FutureConversion._
  import BasicTypes.{isBasic,basicOut}

  var uid : Int = 0

  def start() {
    tree.traverse(defAsyncEnter, new ModifyAsyncContext(null), defAsyncLeave)
  }

  def defAsyncEnter(x : AstTraversable, con : ModifyAsyncContext) : Unit = {
    x match {
      case m : DefAst if m.async => con.m = m ;
      case m : NewAst if consFuture(m) =>
        replace(m,InternalNode("Generic((void*)(_n__Future::wrapCkFuture(CkCreateFuture())))"))
      case m : ReturnAst if con.m != null => {
        var og = m.expr.get match {
          case BinOp(List(FuncCall(_,List(n))),_) => n
        }
        og = if (isBasic(og.etype.get._1)) InternalGroup(List(InternalNode("Generic("),og,InternalNode(")"))) else og
        val ig = InternalGroup(List(
          InternalNode("{"),
          InternalNode(s"ValueMsg *msg = new ValueMsg();"),
          AssignAst(InternalNode("msg->value"),og),
          InternalNode("CkSendToFuture(f, msg);"),
          InternalNode("return;"),
          InternalNode("}")
        ))
        replace(m,ig)
      }
      case bn@BinOp(List(fut@_,FuncCall(t@Type(Identifier(List("SET")),_,_),arg@_)),".") if callsFuture(t) => {
        val f = fut match {
          case id : Identifier => s"_decl_${id.lst.last}"
        }
        val ig = InternalGroup(List(
          InternalNode("{"),
          InternalNode("ValueMsg *msg = new ValueMsg();"),
          AssignAst(InternalNode("msg->value"),arg.head),
          InternalNode(s"CkSendToFuture($f, msg);"),
          InternalNode("}")
        ))
        val encl = findEnclosing(bn) match {
          case Some(x) => x
        }
        val slst = encl.escope.get.slst
        slst.insert(slst.indexOf(encl),ig)
        replace(bn,InternalNode("/* set value of future */"))
      }
      case bn@BinOp(List(_,fc@FuncCall(_,_)),".") if callsAsync(fc) => {
        val mdBn = BinOp(List(bn.lhs.head,imprint(FuncCall(fc.t,fc.params:+InternalNode(s"f$uid")),fc)),".")
        val mkFt = InternalGroup(List(InternalNode(s"CkFuture f$uid = CkCreateFuture();"),imprint(mdBn,bn)))
        val wrap = imprint(InternalNode(s"Generic((void*)(_n__Future::wrapCkFuture(f$uid)))"),bn)
        val encl = findEnclosing(bn) match {
          case Some(x) => x
        }
        val slst = encl.escope.get.slst
        slst.insert(slst.indexOf(encl),mkFt)
        replace(bn,wrap)
        uid += 1
      }
      case _ => ;
    }
  }

  def defAsyncLeave(x : AstTraversable, con : ModifyAsyncContext) : Unit = {
    x match {
      case m : DefAst if m.async => con.m = null ;
      case _ => ;
    }
  }
}