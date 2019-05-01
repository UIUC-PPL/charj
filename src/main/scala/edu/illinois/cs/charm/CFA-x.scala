package edu.illinois.cs.charm

package ControlFlow {
  import scala.collection.mutable.{Map,ListBuffer,Set,Stack}

  object IdGenerator {
    private var counter = 0
    def uniqueId : Int = { counter += 1; counter }
  }

  class Edge(val from : Node, val to : Node) {
    override def toString(): String = s"${from.id} -> ${to.id}"

    def ==(o : Edge) : Boolean = (o.from eq from) && (o.to eq to)
  }

  class Node(val id : String = IdGenerator.uniqueId.toString, val slst : ListBuffer[AstTraversable] = ListBuffer()) {
    val out : Set[Edge] = Set()
    val in  : Set[Edge] = Set()
    var label : Option[String] = None;
    var seen : Boolean = false;
    var g : Option[Graph] = None;

    def +=(s : AstTraversable) : Node = {
      slst += s
      return this
    }

    def ++=(node : Node) : Node = {
      slst ++= node.slst
      return this
    }

    def size : Int = slst.size

    def isEmpty : Boolean = slst.isEmpty

    def dotLabel : String = {
      val lt = if (isEmpty) id else slst.map(StringContext treatEscapes _.toString).mkString("\\n")
      s"""label="$lt"""" + (label match {
        case Some(l) => s""", xlabel="$l""""
        case None    => ""
      })
    }

    def interval() : Set[Node] = {
      val i = Set(this);
      var t : Set[Node] = null;
      do {
        t = (Graph.succ(i).diff(i)).filter(n => (Graph.pred(Set(n)) subsetOf i))
        i ++= t
      } while (!t.isEmpty)
      return i
    }

    def reaches(n : Node, without : Node) : Boolean = reaches(n, Set(without))

    def reaches(n : Node, without : Set[Node] = Set.empty) : Boolean = {
      val seen  = Set[Node]()
      val stack = Stack(this)

      while (!stack.isEmpty) {
        val m = stack.pop
        if (m eq n) {
          return true
        } else {
          seen += m
          // Push all unseen children not in without onto the stack
          stack.pushAll(m.out.map(_.to)
                         .filter(n => !(seen.contains(n) || without.contains(n))))
        }
      }

      return false
    }

    // returns true if n reaches any of the nodes in the set without
    // going through without
    def reachesAny(ns : Set[Node], wo : Set[Node] = Set.empty) : Boolean = {
      ns.exists(this.reaches(_, wo))
    }

    def dom(y : Node) : Boolean = g.get.dom(this, y)

    def sdom(y : Node) : Boolean = !(this eq y) && g.get.dom(this, y)

    override def toString: String = s"Node(${slst.mkString(",")})#$id"
  }

  object Graph {
    def pred(n : Set[Node]) : Set[Node] = {
      return n.map(_.in).flatten.map(_.from)
    }

    def succ(n : Set[Node]) : Set[Node] = {
      return n.map(_.out).flatten.map(_.to)
    }
  }

  class Graph {
    val start = new Node("start")
    val end   = new Node("end")
    var nodes = Set(start, end)

    private[this] val doms = Map[Node,Set[Node]]()

    def dom(x : Node, y : Node) : Boolean = {
      if ((doms contains x) && (doms contains y)) {
        return doms(y) contains x
      } else {
        doms.clear()

        doms += (start -> Set(start))
        (nodes - start).foreach(n => (doms += (n -> nodes)))

        var changesMade = true;
        while (changesMade) {
          changesMade = false;

          for (n <- (nodes - start)) {
            // {n} union with intersection over Dom(p) for all p in pred(n)
            val ds = Graph.pred(Set(n)).map(doms(_)).reduce((x, y) => x & y) + n
            if (doms(n) != ds) {
              doms += (n -> ds)
              changesMade = true;
            }
          }
        }

        return dom(x, y)
      }
    }

    def markUnseen() : Unit = {
      for (node <- nodes) {
        node.seen = false;
      }
    }

    override def toString(): String = {
      return "digraph g {" + nodes.map(_.out.map("\n" + _.toString + ";").mkString("")).mkString("") + nodes.map(node => s"""\n${node.id} [${node.dotLabel}]""").mkString + "\n}"
    }

    def contains(node : Node) : Boolean = nodes.contains(node)

    def insert(from : Node, to : Node) : Edge = {
      assert(contains(from) || contains(to))
      assert(!(to eq start) && !(from eq end))
      var e = new Edge(from, to)
      from.out += e
      to.in    += e
      nodes    += from
      nodes    += to
      from.g    = Some(this)
      to.g      = Some(this)
      e
    }

    def prepend(e : Edge, n : Node) : Edge = {
      remove(e)
      insert(e.from, n)
      insert(n, e.to)
    }

    def remove(node : Node) : Unit = {
      assert(contains(node) && !((node eq start) || (node eq end)))
      // Disconnect all the in-nodes from this node
      for (in <- node.in) {
        in.from.out -= in
      }
      // Disconnect all of the out-nodes from this node
      for (out <- node.out) {
        out.to.in -= out
      }
      // remove it from the nodes list
      nodes -= node
    }

    def remove(edge : Edge) : Unit = {
      edge.to.in    -= edge
      edge.from.out -= edge
    }

    def split(n : Node, idx : Int) : Node = {
      assert(contains(n) && (idx > 0 && idx < n.size))
      // Create a new child node containing the rest of the statements
      val child = new Node(slst = n.slst.slice(idx, n.slst.size))
      for (out <- n.out) {
        // Then, connect each out-edge to it instead
        insert(child, out.to)
        // Removing the original edge from the graph
        remove(out)
      }
      // Then, connect the original node to the new child node
      insert(n, child)
      // And remove all of the moved statements from the original node
      n.slst.trimEnd(n.slst.size - idx)
      // Finally, return the new child node
      child
    }

    // Removes empty nodes and merges nodes when it can
    def prune() : Unit = {
      for (n <- nodes
           if !(n eq start); if !(n eq end)) {
        // If a node is empty
        if (n.isEmpty) {
          // Map all of the node's ins to its outs
          for (in <- n.in) {
            for (out <- n.out) {
              insert(in.from, out.to)
            }
          }
          // Then remove it
          remove(n)
        }
      }
      var madeChanges : Boolean = false
      do {
        madeChanges = false
        for (n <- nodes
             if !(n eq start); if !(n eq end)) {
          // If a node only has one child
          if (n.out.size == 1) {
            val to = n.out.head.to
            // And the child's only parent is that node
            if (to.in.size == 1 && !((to eq start) || (to eq end))) {
              // Copy all of the statements from the child
              n ++= to
              // All of the child's children because the node's children
              for (out <- to.out) {
                insert(n, out.to)
              }
              // Then remove the child
              remove(to)
              // Mark that we made changes
              madeChanges = true
            }
          }
        }
      } while (madeChanges)
    }
  }

  class Analysis(tree : AstTraversable, verbose : Boolean) {

    def start() {
      visit(tree)
    }

    private def visitScope(s : Scope, g : Graph, n : Node) : List[Node] = {
      var ns = List(n)
      for (st <- s.slst) {
        if (ns.length == 1) {
          ns = visit(st, g, ns.head)
        } else {
          val t = new Node()
          ns.map(g.insert(_, t))
          ns = List(t)
        }
      }
      return ns
    }

    // TODO, we need a way to account for "return" statements (break and continue are not defined)
    // TODO, this would not acccount for predicated expressions if they existed
    def visit(x : AstTraversable, g : Graph = null, n : Node = null) : List[Node] = {
      x match {
        case s : DefAst => {
          val g  = new Graph()
          val n  = new Node()
          g.insert(g.start, n)
          var ns = visitScope(s, g, n)
          ns.map(g.insert(_, g.end))
          g.prune()
          if (verbose) println("found " + g + " for " + s)
          s.cfg = Some(g)
          List()
        }
        case s : Scope => {
          if (g == null || n == null) {
            s.slst.map(visit(_))
            List()
          } else {
            visitScope(s, g, n)
          }
        }
        case x : DoWhileAst => {
          // Create the body
          val body  = new Node()
          // Connecting it to the previous node, then visiting it
          g.insert(n, body)
          var ns    = visit(x.body, g, body)
          val cond  = new Node()
          // Connect all of the body terminal nodes to the test expression
          ns.map(g.insert(_, cond))
          // Connnect the test expression back to the body
          g.insert(cond, body)
          // Then, visit the test expression
          visit(x.cond, g, cond)
        }
        case x : ForAst => {
          // Visit the setup first
          val decls = new Node()
          g.insert(n, decls)
          x.decls.map(visit(_, g, decls))
          // Visit the test expression
          val expr1 = new Node()
          g.insert(decls, expr1)
          x.expr1.map(visit(_, g, expr1))
          // Visit the body (saving the terminal node(s))
          val body  = new Node()
          g.insert(expr1, body)
          val ns    = visit(x.body, g, body)
          // Visit the increment
          val expr2 = new Node()
          x.expr2.map(visit(_, g, expr2))
          // Which connects back to the test expression
          g.insert(expr2, expr1)
          // Each body node connects to the increment
          ns.map(g.insert(_, expr2))
          // Add a node for everything after the loop
          val empt = new Node()
          g.insert(expr1, empt)
          // The terminal node is the empty expression
          List(empt)
        }
        case x : IfAst => {
          n += x.expr
          val t    = new Node()
          g.insert(n, t)
          var next = visit(x.then, g, t)
          if (x.el.isDefined) {
            val f  = new Node()
            g.insert(n, f)
            next ++= visit(x.el.get, g, f)
          } else {
            next = n :: next
          }
          next
        }
        case x : WhileAst => {
          // Visit the test expression
          val cond = new Node()
          g.insert(n, cond)
          visit(x.cond, g, cond)
          // Visit the body
          val body = new Node()
          g.insert(cond, body)
          val ns = visit(x.body, g, body)
          // Connect all of the body terminal nodes to the test expression
          ns.map(g.insert(_, cond))
          // Add a node for everything after the loop
          val empt = new Node()
          g.insert(cond, empt)
          // The terminal node is the empty expression
          List(empt)
        }
        case x : ReturnAst => {
          assert(n != null)
          // Add the return exp to the current node
          n += x
          // Then connect it to the end of the graph
          g.insert(n, g.end)
          // This branch has no additional terminal nodes
          List()
        }
        case _ => {
          if (verbose) println("warning, no match for " + x.getClass)
          if (n != null) n += x
          List(n)
        }
      }
    }
  }
}
