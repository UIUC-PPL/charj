package edu.illinois.cs.charm

import scala.collection.mutable.Set

// TODO This is really naive right now, will need to be improved at some point.
object Pass {
    def runAll(cls : List[Class[_ <: Pass]], tree : AstTraversable,
               past : Set[Class[_ <: Pass]] = Set.empty) : Unit = {
        cls.foreach(run(_,tree,past))
    }

    def run(c : Class[_ <: Pass], tree : AstTraversable,
            past : Set[Class[_ <: Pass]] = Set.empty) : Unit = {
        val n = c.newInstance
        runAll(n.reqd.filter(c => !past.contains(c)), tree, past)
        n.tree = tree
        n.start
        past += c
    }
}

abstract class Pass(val reqd : List[Class[_ <: Pass]]) {
    def start : Unit
    def verbose : Boolean = Parse.verbose
    var tree : AstTraversable = null
}