package CharjParser

// Temporary Node and CFG classes. Ralf will define the actual versions.
class CFG {
  var start : Node
  var nodes : Set
}

class Node {
  var succ, pred : Set
  var in, out : Set
  var gen, kill : Set
}

// TODO: Define BOT/TOP
// TODO: Define whether it is forwards or backwards
class DataAnalysis(conf : Set => Set,
                   fUniverasl : Node => Set,
                   fGen : Node => Set,
                   fKill : Node => Set) {
  var U : Set

  /* INITIALIZATION
   * For: Each node, n, in the CFG
   *   Compute U, the universal set
   *   Compute fKill(n), fGen(n)
   *   Set initial in(n), out(n)
   * End INITIALIZATION
   *
   * ANALYSIS
   * While: out(n) keeps changing
   *   For: Each node, n, in the CFG
   *     in(n) = conf(out(n')) for all n' predecessors
   *     out(n) = gen(n) U (in(n) - kill(n))
   *   End for
   * End ANALYSIS
   */

  def runOn(cfg : CFG) {
    initialize(cfg);
    analyze(cfg);
  }

  def initialize(cfg : CFG) {
    // Generate the universal set for this CFG
    U.clear()
    cfg.nodes.foreach {
      U ++= fUniversal(_)
    }
    // For every node, compute it's gen, kill, and initial out sets
    cfg.nodes.foreach {
      _.gen   = fGen(_)
      _.kill  = fKill(_)
      _.out   = U -- _.kill
    }
    // Set the in and out of the start node
    start.in.clear()
    start.out = start.gen
  }

  def analyze(cfg : CFG) {
    var changed : Boolean = true
    var oldOut : Set
    while (changed) {
      changed = false
      cfg.nodes.foreach {
        _.in = conf(_.pred)
        oldOut = _.out.clone()
        _.out = _.gen | (_.in -- _.kill)
        if (oldOut <> _.out ) {
          changed = true
        }
      }
    }
  }
}
