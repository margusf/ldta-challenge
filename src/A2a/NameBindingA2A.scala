package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.SourceMessage

object NameBindingA2A extends NameBindingA1 {
    override def initialEnv = EnvA2A.initialEnv

    override def processDeclarations(decl: Declarations,
                                     env: EnvBase): EnvA2A = {
        val newEnv = super.processDeclarations(decl, env).asInstanceOf[EnvA2A]

        for (proc <- decl.procedures) {
            doProcedure(proc, newEnv)
        }

        newEnv.addProcedures(procedureNames(decl))
    }

    def doProcedure(proc: ProcedureDecl, env: EnvA2A) {
        if (proc.name != proc.name2) {
            throw new NameError(proc.name)
        }

        val params =
            for (fp <- proc.params; id <- fp.ids.ids)
                yield id

        val procEnv = processDeclarations(
                proc.decl,
                env.addProcedures(List(proc.name)).addVars(params))
        processStatements(proc.body, procEnv)
    }

    override def getIdList(decl: Declarations) =
        super.getIdList(decl) ++ procedureNames(decl)

    def procedureNames(decl: Declarations) =
        decl.procedures.map(_.name)

    override def processStatement(stm: Statement, envBase: EnvBase) {
        val env = envBase.asInstanceOf[EnvA2A]

        stm match {
            case ProcedureCall(name, args) =>
                env.checkProc(name)
                args.foreach(processExpr(_, env))
            case other =>
                super.processStatement(other, env)
        }
    }
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

    def checkProc(id: Id) {
        check(id, getProc(id.text))
    }

    protected def getProc(name: String): Option[Id] =
        if (procs.contains(name))
            Some(procs(name))
        else
            parent.getProc(name)

    override def toString = "[" + defs + ", " + procs + "] => " + parent
}

object EnvA2A {
    val preProcs = Map(
        "Write" -> Id("Write"),
        "WriteLn" -> Id("WriteLn"),
        "Read" -> Id("Read")
    )

    val initialEnv =
        new EnvA2A(null, Map.empty, Map.empty, Map.empty) {
            override def get(name: String) = EnvA1.predefs.get(name)
            override def getType(name: String) = EnvA1.preTypes.get(name)
            override def getProc(name: String) = preProcs.get(name)
            override def toString = "()"
        }
}