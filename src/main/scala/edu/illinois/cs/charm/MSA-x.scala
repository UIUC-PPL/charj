package edu.illinois.cs.charm

import scala.collection.mutable.{HashMap,Iterable,ListBuffer,Stack,Set,Queue}
import scala.util.parsing.input.{Positional,Position}

object Phase extends Enumeration {
  protected case class Val(val accessor: String) extends super.Val { }

  implicit def valueToPhaseValue(x: Value) = x.asInstanceOf[Val]

  val Read    = Val("get")
  val Write   = Val("set")
  val Unknown = Val("invalid")
  val Accum   = Val("accumulate")
}

class PhaseAnalysis(tree : AstTraversable, verbose : Boolean) {
  import MSAHelper._;

  implicit class SyncNode(val n : ControlFlow.Node) {
    def op(msa : String) : Option[String] = {
      n.label match {
        case Some(s) if (s contains msa) => {
          val i = s.indexOf(msa) + msa.length + 1
          val j = { var t = s.indexOf(",", i);
                    if (t > 0) t else s.length }
          Some(s.substring(i, j))
        }
        case _ => None
      }
    }

    def phase(msa : String) : Phase.Value = {
      op(msa) match {
        case Some(psi) if psi.startsWith("Psi") => Phase.withName(psi.substring(psi.indexOf(">") + 1, psi.length - 1).trim)
        case Some("Iota") => Phase.Unknown
        case Some(s) => Phase.withName(s)
        case None => Phase.Unknown
      }
    }

    def isSync(msa : String) : Boolean = {
      op(msa) match {
        case Some(s) => (s == "Iota" || s.startsWith("Psi"))
        case None    => false
      }
    }

    def isPsiFrom(msa : String, p : Phase.Value) : Boolean = {
      op(msa) match {
        case Some(s) => s.startsWith(s"Psi($p")
        case None    => false
      }
    }

    def +=(op : (String, String)) : Unit = {
      n.label match {
        case Some(s) => {
          n.label = Some(s + s",${op._1}=${op._2}")
        }
        case _ => {
          n.label = Some(s"${op._1}=${op._2}")
        }
      }
    }
  }

  class LabelContext {
    val sstk  = Stack[AstTraversable]()
    var label = Map[String, String]()
  }

  def start() {
    tree.traverse(contextStart, null, null)

    val a = new ControlFlow.Node("a");
    val b = new ControlFlow.Node("b");
    val c = new ControlFlow.Node("c");
    val d = new ControlFlow.Node("d");

    val g = new ControlFlow.Graph;
    g.insert(g.start, a)
    g.insert(a, b)
    g.insert(a, c)
    g.insert(a, g.end)
    g.insert(b, d)
    g.insert(c, d)
    g.insert(d, a)

    println(g)
    println("d reaches the end " + d.reaches(g.end))
    println("d reaches the end without a " + d.reaches(g.end, a))
    println("b dom d? " + b.dom(d) + ". a dom d? " + a.dom(d))
  }

  def mkPsi(msa : String, x : ControlFlow.Node, y : ControlFlow.Node) : ControlFlow.Node = mkPsi(msa, x.phase(msa), y.phase(msa))

  def mkPsi(msa : String, x : Phase.Value, y : Phase.Value) : ControlFlow.Node = {
    val psi = new ControlFlow.Node(slst = ListBuffer(new InternalNode(s"$msa.syncTo$y()")))
    psi += (msa -> s"Psi($x -> $y)")
    return psi
  }

  def mkIota(msa : String) : ControlFlow.Node = {
    val iota = new ControlFlow.Node(slst = ListBuffer(new InternalNode(s"$msa.idleBarrier()")))
    iota += (msa -> "Iota")
    return iota
  }

  def skipMatches(msa : String, es : Iterable[ControlFlow.Edge], from : Phase.Value, exits : Set[ControlFlow.Edge]) : Set[ControlFlow.Edge] = {
    if (from == Phase.Unknown) return es.to[Set]

    val queue = es.to[Queue]
    val out   = Set[ControlFlow.Edge]()

    while (!queue.isEmpty) {
      val curr = queue.dequeue
      assert(!exits.contains(curr))
      val anyExits = curr.to.out.exists(exits.contains(_))
      if (!anyExits && !curr.to.seen && !curr.to.isSync(msa) && curr.to.phase(msa) == from) {
        if (curr.to.out.size > 1 && curr.to.out.exists(e => e.to.phase(msa) != from)) {
          curr.to.out.foreach(e => {
            if (!e.to.isSync(msa) && e.to.phase(msa) == from) {
              out ++= e.to.out
            } else {
              out += e
            }
          })
        } else {
          curr.to.out.foreach(queue.enqueue(_))
        }
      } else {
        out += curr
      }
    }

    return out
  }

  def reducePhases(msa : String, ps : Iterable[ControlFlow.Edge], p : Phase.Value) : Option[Phase.Value] = reducePhases(msa, ps, Some(p))

  def reducePhases(msa : String, ps : Iterable[ControlFlow.Edge], p : Option[Phase.Value] = None) : Option[Phase.Value] = {
    val pss = ps.map(_.from.phase(msa)).toSet
    if ((p match {
      case Some(p) => pss - p
      case None    => pss
    }).isEmpty) None
    else if (pss.size == 1) Some(pss.head)
    else Some(Phase.Unknown)
  }

  def findExits(start : ControlFlow.Node, header : Set[ControlFlow.Node], back : Set[ControlFlow.Edge]) : Set[ControlFlow.Edge] = {
    val seen  = header.clone
    val queue = Queue(start)
    val pending = Set[ControlFlow.Edge]()

    // the back edges set is unaffected by this routine, this merely stops it
    // from looping superfluously if it's provided an empty list of backedges
    while (!back.isEmpty && !queue.isEmpty) {
      val curr = queue.dequeue
      seen += curr
      // look for all of the unseen children
      val children = curr.out.filter(out => !seen.contains(out.to))
      // find the children that do not reach the back edges w/o going through the header
      val exits = children.filter(out => !out.to.reachesAny(back.map(_.from), header))
      // add them to the pending array, removing any nodes that use them as exits
      pending --= pending.filter(out => out.to.reachesAny(exits.map(_.from), header))
      pending ++= exits
      // then visit all of the unseen children
      (children -- exits).foreach(out => queue.enqueue(out.to))
    }

    pending
  }

  def phasePropagation(g : ControlFlow.Graph, msa : String, start : Iterable[ControlFlow.Node],
                       loops : (Set[ControlFlow.Node], Set[ControlFlow.Edge]) = (Set(), Set())) : Unit = {
    val queue = start.to[Queue]
    val seen  = Set[ControlFlow.Node]()
    while (!queue.isEmpty) {
      val curr = queue.dequeue
      seen += curr

      val back = curr.in.filter(n => curr.dom(n.from))
      val exits = findExits(curr, loops._1 + curr, back)

      if (!back.isEmpty) {
        phasePropagation(g, msa, back.map(t =>
          curr.out.map(_.to).filter(n => n.reaches(t.from, curr))
        ).flatten.to[Set], (loops._1 + curr, exits))
      }

      if (curr.phase(msa) == Phase.Unknown ||
          curr.isPsiFrom(msa, Phase.Unknown)) {
        val in = reducePhases(msa, curr.in)
        if (in.isDefined && in.get != Phase.Unknown) {
          if (curr.isSync(msa)) {
            // update incoming
          } else {
            // assign temporary label
            // if there are any backedges, propagate to them
          }
        }
      }
    }
  }

  def phaseAnalysis(g : ControlFlow.Graph, msa : String, start : Iterable[ControlFlow.Node],
                    loops : (Set[ControlFlow.Node], Set[ControlFlow.Edge]) = (Set(), Set())) : Unit = {
    // we use a queue to traverse the graph breadth-first
    val queue = start.to[Queue]
    var branches = start.flatMap(_.in)
                        .filter(e => !(e.to.dom(e.from) || loops._2.contains(e))).to[Set]
    while (!queue.isEmpty) {
      val curr = queue.dequeue
      val currPhase = curr.phase(msa)
      // mark the node as seen
      curr.seen = true
      // find any back edges
      val back  = curr.in.filter(n => curr.dom(n.from))
      val exits = findExits(curr, loops._1 + curr, back)
      // if this is not a psi node or iota barrier
      if (!curr.isSync(msa) && currPhase != Phase.Unknown) {
        // if any incoming edges do not match our phase
        reducePhases(msa, curr.in -- back, currPhase) match {
          case Some(in) => {
            // prepend a psi node onto the incoming edge, skipipng matches
            branches = skipMatches(msa, branches, in, loops._2).map(e => g.prepend(e, mkPsi(msa, in, currPhase)))
          }
          case _ => ;
        }
        // if any outgoing, back edges do not match our phase
        reducePhases(msa, back, currPhase) match {
          case Some(bp) => {
            // prepend a psi node onto them
            back.foreach(e => g.prepend(e, mkPsi(msa, bp, currPhase)))
          }
          case _ => ;
        }
      }
      // remove all of the branches that point to this node
      for (branch <- branches) {
        if (curr eq branch.to) {
          branches -= branch
        }
      }
      val outt = curr.out.clone
      // recurse into a natural loop’s body
      if (!back.isEmpty) {
        phaseAnalysis(g, msa, back.map(t =>
          curr.out.map(_.to).filter(n => n.reaches(t.from, curr))
        ).flatten.to[Set], (loops._1 + curr, exits))
        outt ++= exits
      }
      // for each outgoing edge that is unseen and not an exit node
      for (out <- outt.filter(e => (!e.to.seen && !loops._2.contains(e)))) {
        // if no enqueued node reaches the node without going through the header
        if (!queue.exists(_.reaches(out.to, loops._1))) {
          // enqueue it
          queue.enqueue(out.to)
        }
        // regardless, add the outgoing branch to the branches list
        branches += out
      }
      // if there were any loop exits
      if (!exits.isEmpty) {
        // prepend an iota onto all of the branches that do not reach an exit
        // (without going through an outer-loop if nested)
        branches = branches.map(e => if (e.to.reachesAny(exits.map(_.from), loops._1)) e else g.prepend(e, mkIota(msa)))
        // then, prepend an iota onto all of the exits
        // (exits -- branches).foreach(e => g.prepend(e, mkIota(msa)))
      }
    }
  }

  def labelCFG(g : ControlFlow.Graph) : Set[String] = {
    var foundMSA = Set[String]()
    val stack : Stack[ControlFlow.Node] = Stack[ControlFlow.Node](g.start)

    g.markUnseen()

    while (!stack.isEmpty) {
      val n = stack.pop
      var i = 0

      while (i < n.slst.size) {
        val s   = n.slst(i)
        val con = new LabelContext

        s.traverse(visit, con, null)

        // TODO ensure correctness for multiple MSA's

        for ((arr, op) <- con.label) {
          if(op contains "sync") {
            val idx = n.slst.indexOf(s)
            val psi = if (idx == 0) n else g.split(n, idx)
            if (psi.slst.size != 1) {
              g.split(psi, 1)
            }
            psi += (arr -> s"Psi(${Phase.Unknown} -> ${op.substring(6)})")
            assert((i + 1) >= n.size)
          } else {
            n.op(arr) match {
              case Some(other) if other != op => {
                val idx = n.slst.indexOf(s)
                assert(idx != 0)
                val nxt = g.split(n, idx)
                nxt += (arr -> op)
                assert((i + 1) >= n.size)
              }
              case None => {
                n += (arr -> op)
              }
              case _ => ;
            }
          }
        }

        foundMSA ++= con.label.keySet
        i += 1
      }

      if (!n.seen) {
        n.seen = true
        for (out <- n.out) {
          if (!out.to.seen) {
            stack.push(out.to)
          }
        }
      }
    }

    foundMSA
  }

  def contextStart(x : AstTraversable, con : Any) : Unit = {
    x match {
      case d : DefAst if d.cfg.isDefined => {
        val g = d.cfg.get
        val foundMSA = labelCFG(g)
        for (msa <- foundMSA) {
          if (!ignoreMSA(d, msa)) {
            g.markUnseen
            phaseAnalysis(g, msa, Iterable(g.start))
            println(g)
          }
        }
      }
      case _ => ;
    }
  }

  def tryLabel(con : LabelContext, arr : String, newLabel : String, suppress : Boolean = false) : Unit = {
    if (!(con.label contains arr) || (con.label(arr) == newLabel)) {
      con.label += (arr -> newLabel)
    } else if (!suppress) {
      throw new RuntimeException("Overlapping uses of an MSA are not supported!")
    }
  }

  def visit(x : AstTraversable, con : LabelContext) : Unit = {
    x match {
      case BinOp(Identifier(i :: Nil) :: FuncCall(t, _) :: _, ".") => {
        val etype = x.asInstanceOf[BinOp].lhs.head.etype
        // TODO do more rigorous type-checking
        val isMSA = etype.toString.contains("MSA")
        if (t.toString == "[]" && isMSA) {
          for (parent <- con.sstk) {
            parent match {
              case BinOp(y :: FuncCall(op, _) :: Nil, ".") if x eq y => {
                if (op.toString == "+=" || op.toString == "-=") {
                  tryLabel(con, i.toString, Phase.Accum.toString)
                }
              }
              case AssignAst(y, _) if x eq y => tryLabel(con, i.toString, Phase.Write.toString)
              case _ => ;
            }
          }
          // When no label was added, add a "read" label
          tryLabel(con, i.toString, Phase.Read.toString, true)
        } else if (t.toString.contains("sync") && isMSA) {
          tryLabel(con, i.toString, t.toString)
        }
      }
      case _ => ;
    }
    con.sstk push x
  }
}

object MSAHelper {
  def isLocalAccess(x : AstTraversable, encl : Option[AstTraversable], s : Scope) : Boolean = {
    val alst = encl match {
      case Some(y) if (s eq y) => x.alst
      case Some(y)             => y.alst
      case None                => List[Annotation]()
    }
    alst.exists(a => a.id == "local")
  }

  private def hasMSA(lst : List[AstTraversable], msa : String) : Boolean = {
    lst match {
      case x :: rest => {
        (x match {
          case Identifier(ids) => ids.contains(msa)
          case _ => false
        }) || hasMSA(rest, msa)
      }
      case Nil => false
    }
  }

  def ignoreMSA(x : DefAst, msa : String) : Boolean = {
    x.alst.exists(a => a.id == "ignore" && (a.params.isEmpty || hasMSA(a.params, msa)))
  }
}

class HandleConversion(tree : AstTraversable, verbose : Boolean) {
  import MSAHelper._;

  class HandleContext(val h : String, val p : Phase.Value)
  class ConversionContext(var m : HashMap[String, HandleContext], var x : Option[Scope])

  val autoTy    = new Type(new Identifier(List("auto")),Nil,None)
  val autoIdent = new ScopeIdent(List(SystemIdentifier.namespace, "auto"))

  autoTy.system = true

  def start() {
    val c = new ConversionContext(new HashMap(), None)
    tree.traverse(contextStart, c, contextEnd)
  }

  def contextStart(x : AstTraversable, con : ConversionContext) : Unit = {
    x match {
      case s : Scope => {
        con.x = Some(s)
      }
      case BinOp(Identifier(i :: Nil) :: FuncCall(t, innerRest) :: outerRest, ".") if con.x.isDefined => {
        def updateHandle (i: String, p : Phase.Value) : Boolean = {
          val n = NameGenerator.createName.substring(1)
          val op : AstTraversable = if (con.m isDefinedAt i) {
            var h = con.m(i).h
            con.m(i) = new HandleContext(n, p)

            new InternalNode(s"_decl_$h.syncTo$p()")
          } else {
            con.m += (i -> new HandleContext(n, p))

            var id = Identifier(List(s"getInitial$p"))
            id.eprev = Some(x.asInstanceOf[BinOp].lhs.head)
            val prevType = CodeGeneration.testTemplateType(id)

            new InternalNode(s"((${prevType._2}*)(_decl_$i.t.v))->_def_getInitial$p()")
          }

          ModifyASTReplace.replace(x, new DeclAst(true, new TypeParamAst(n, autoTy), Some(op)))
        }

        def updateAccess() : Boolean = {
          val encl = ModifyASTReplace.findEnclosing(x)
          val accs = if (con.m(i).p == Phase.Read && isLocalAccess(x, encl, con.x.get)) "get2" else con.m(i).p.accessor
          val repl = ModifyASTReplace.mkMSAReplacement(s"_decl_${con.m(i).h}.$accs", innerRest, outerRest)
          ModifyASTReplace.replace(x, repl, encl)
        }

        val etype = x.asInstanceOf[BinOp].lhs.head.etype
        // TODO check to see if replacement failed
        // TODO do more rigorous type-checking
        val succ = (etype.toString.contains("MSA")) && (t.toString match {
          case "syncToRead" => {
            updateHandle(i, Phase.Read)
          }
          case "syncToWrite" => {
            updateHandle(i, Phase.Write)
          }
          case "syncToAccum" => {
            updateHandle(i, Phase.Accum)
          }
          case "[]" if (con.m isDefinedAt i) => {
            updateAccess()
          }
          case _ => false;
        })

        if (verbose && succ) println("successfully updated MSA reference....")
      }
      case _ => ;
    }
  }

  def contextEnd(x : AstTraversable, con : ConversionContext) : Unit = {
    con.x match {
      case Some(y) if x eq y => {
        con.m = new HashMap()
        con.x = None
      }
      case _ => ;
    }
  }
}
