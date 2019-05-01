package edu.illinois.cs.charm

// TODO generate body of receivers (might do in secondary pass)
// TODO generate SDAG code to call receivers
// TODO add UID to receivers so they can be overloaded (???)

class DefineWhenEntries extends Pass(List()) {
  private var uid : Int = -1

  import ModifyASTReplace.{mkTupleType,mkTupleGet,mkTuple}
  import DetermineEnclose.{setEnclosing,setEnclosingRec}

  private def mkReceiver(ecls : ClassAst, i : Identifier, params : List[TypeParamAst], f : String) : Unit = {
    val unty = Type(Identifier(List("unit")),List(),None)
    val ilst = params.map(x => Identifier(List(x.str)))
    val body = List(BinOp(List(Identifier(List(f)), FuncCall(Type(Identifier(List("set")), List(), None), List(mkTuple(ilst)))), "."))
    val sdef = DefAst(List("entry"),None,Type(i,List(),None),params,unty,Some(body))
    body.foreach(setEnclosingRec(_,Some(ecls),Some(sdef)))
    params.foreach(setEnclosing(_,Some(ecls),None))
    ecls.slst.prepend(setEnclosing(sdef, Some(ecls), None))
  }

  def start : Unit = {
      tree.traverse(enter,null,null)
  }

  def enter(x : AstTraversable, con : Any) : Unit =
    x match {
      case m : WhenAst =>
        val ecls = m.ecls.get
        val edef = m.edef match {
          case Some(d) if d.entry && !d.cons => d
          case _ => throw new RuntimeException("when can only be used in non-cons entry methods")
        }

        if (m.entries.length != 1 ||
            m.entries.head.pattern.length != 1) {
          throw new RuntimeException("multiple entries and/or parameters are not yet supported")
        }

        m.entries.foreach(entry => {
          // TODO fully clone
          val params = entry.pattern.map {
            case p: TypeParamAst =>
              TypeParamAst(p.str, p.param)
            case p: PlaceholderAst =>
              uid += 1
              TypeParamAst(s"hldr$uid", Type(Identifier(List("any")), List(), None))
            case _ => throw new RuntimeException("expressions are not yet supported")
          }
          uid += 1
          val fty1 = Type(Identifier(List("Future")),List(mkTupleType(params.map(_.param))),None)
          val fty2 = Type(Identifier(List("Future")),List(mkTupleType(params.map(_.param))),None)
          val call = FuncCall(fty2,List())
          val decl = DeclAst(dtype = false,TypeParamAst(s"ft$uid",fty1),Some(NewAst(call,List())))
          call.skipGen = true
          ecls.slst.prepend(setEnclosingRec(decl, Some(ecls), None))
          mkReceiver(ecls,entry.ident,params,s"ft$uid")
          m.slst.prependAll(params.zipWithIndex.map({{
            case (TypeParamAst(s,ty),i) => setEnclosingRec(
              DeclAst(dtype = false,TypeParamAst(s,ty),
                Some(mkTupleGet(BinOp(List(Identifier(List(s"ft$uid")),
                FuncCall(Type(Identifier(List("get")),List(),None),List())),"."),i))),
              Some(ecls),Some(edef)
            )}}))
        })
      case _ =>
    }
}
