package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.SourceMessage

object NameBindingA2A extends NameBindingA1 {
    override def initialEnv = EnvA2A.initialEnv

    override def process(module: Module): Option[SourceMessage] = {
        try {
            if (module.name1 != module.name2) {
                throw NameError(module.name2)
            }

            val env = processDeclarations(module.decl, initialEnv, true)
            processStatements(module.statements, env)

            None
        } catch {
            case NameError(id: Id) =>
                Some(new SourceMessage(
                    "Invalid identifier: " + id.text, SourceMessage.Error, id))
        }
    }

    def processDeclarations(decl: Declarations,
                                     env: EnvBase,
                                     includeVars: Boolean): EnvA2A = {
        // TODO: refactor this a bit?

        // subEnv is used for checking nested procedures.
        var subEnv = env.asInstanceOf[EnvA2A]

        checkDuplicates(getIdList(decl))

        for (td <- decl.types) {
            checkType(td, subEnv)
            subEnv = subEnv.addType(td.name)
        }

        for (cd <- decl.consts) {
            val cType = processExpr(cd.expr, subEnv)
            subEnv = subEnv.addConst(cd.name)
        }

        for (vd <- decl.vars) {
            checkType(vd.varType, subEnv)
        }

        var withVars = subEnv.addVars(getVarNames(decl))

        for (proc <- decl.procedures) {
            doProcedure(proc, if (includeVars) withVars else subEnv)

            subEnv = subEnv.addProcedures(List(proc.name))
            withVars = withVars.addProcedures(List(proc.name))
        }

        // The procedure body will be checked with environment
        // containing all the vars and sub-procedures.
        withVars
    }

    def doProcedure(proc: ProcedureDecl, env: EnvA2A) {
        if (proc.name != proc.name2) {
            throw new NameError(proc.name)
        }

        val params =
            for (fp <- proc.params; id <- fp.ids.ids)
                yield id

        // Sub-procedures are without proc name and params.
        val procEnv = processDeclarations(proc.decl, env, false)
        // body is processed with variables and params.
        processStatements(proc.body,
            procEnv.addProcedures(List(proc.name)).addVars(params))
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