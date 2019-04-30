package FrontEnd

import scala.collection.mutable.ListBuffer

// TODO add a push to future mechanic so it can follow recursive paths
// TODO make sure it does not traverse entry method borders

class SyncAnalysis(tree : AstTraversable, verbose : Boolean) {
  def start() : Unit = {
    tree.traverse(enter, null, null)
  }

  private def enter(x : AstTraversable, con : Any) =
    x match {
      case m : DefAst if m.entry && !m.threaded => {
        if (m.callsSync.isEmpty) {
          if ($ contains m) {
            throw new RuntimeException("sync analysis found analyzed function w/o sync definition at global level!")
          } else {
            (new DefAnalysis(m,verbose)).start()
          }
        }
      }
      case _ => ;
    }
}

private object $ {
  private val lst : ListBuffer[DefAst] = new ListBuffer()

  def contains(x : DefAst) : Boolean = lst contains x
  def +=(x : DefAst) : Unit = if (!contains(x)) lst += x
}

private class DefAnalysis(tree : DefAst, verbose : Boolean) {
  private var recursive : Boolean = false

  $ += tree

  def start() : Unit = {
    tree.traverse(enter, null, null)

    if (recursive && tree.sync) {
      tree.callsSync = Some(true)
    } else if (tree.callsSync.isEmpty) {
      tree.callsSync = Some(false)
    }
  }
  
  private def enter(x : AstTraversable, con : Any) =
    x match {
      case m : FuncCall => {
        m.t.eres match {
          case Some((_,df@DefAst(_,_,_,_,_,_))) => {
            if ((df.keywords contains "sync") ||
                (df.callsSync.isDefined && df.callsSync.get)) {
              tree.callsSync = Some(true)
            } else if (df.callsSync.isEmpty) {
              if (df eq tree) {
                recursive = true
              } else if ($ contains df) {
                println("WARNING : sync analysis is unable to follow recursive paths!")
              } else {
                (new DefAnalysis(df,verbose)).start()
                tree.callsSync = Some((tree.callsSync.isDefined && tree.callsSync.get) || df.callsSync.get)
              }
            }
          }
          case _ => throw new RuntimeException("unable to analyze call to ${m.t.eres}!")
        }
      }
      case _ => ;
    }
}