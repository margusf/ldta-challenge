package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.SourceMessage

object NameBindingA2A extends NameBindingA1 {
    override def initialEnv = EnvA2A.initialEnv

    override def processDeclarations(decl: Declarations,
                                     env: EnvBase): EnvA2A = {
        var newEnv = super.processDeclarations(decl, env).asInstanceOf[EnvA2A]

        for (proc <- decl.procedures) {
            if (proc.name != proc.name2) {
                throw new NameError(proc.name)
            }
        }

        newEnv
    }

    override def getIdList(decl: Declarations) =
        super.getIdList(decl) ++
            decl.procedures.map(_.name)
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

    def addProcedures(ids: List[Id]) =
        new EnvA2A(this, Map.empty, Map.empty,
                ids.map((id: Id) => id.text -> id).toMap)
}

object EnvA2A {
    val initialEnv =
        new EnvA2A(null, Map.empty, Map.empty, Map.empty) {
            override def get(name: String) = EnvA1.predefs.get(name)
            override def getType(name: String) = EnvA1.preTypes.get(name)
            override def toString = "()"
        }
}