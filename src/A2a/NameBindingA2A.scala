package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.SourceMessage

object NameBindingA2A extends NameBindingA2A {
}

class NameBindingA2A extends NameBindingA1 {
    override def initialEnv = EnvA2A.initialEnv
}

class EnvA2A(parent: EnvA2A,
             defs: Map[String, (Id, Boolean)],
             types: Map[String, Id],
             procs: Map[String, Id])
        extends EnvBase(parent, defs, types) {
    def addVars(ids: List[Id]) = {
        val idMap = ids.map((id: Id) => id.text -> (id, true)).toMap
        new EnvA2A(this, idMap, Map.empty, Map.empty)
    }

    def addConst(id: Id) =
        new EnvA2A(this, Map(id.text -> (id, false)), Map.empty, Map.empty)

    def addType(id: Id) =
        new EnvA2A(this, Map.empty, Map(id.text -> id), Map.empty)
}

object EnvA2A {
    val initialEnv =
        new EnvA2A(null, Map.empty, Map.empty, Map.empty) {
            override def get(name: String) = EnvA1.predefs.get(name)
            override def getType(name: String) = EnvA1.preTypes.get(name)
            override def toString = "()"
        }
}