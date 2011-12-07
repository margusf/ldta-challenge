package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.SourceMessage

object NameBindingA2A extends NameBindingA2A

class NameBindingA2A extends NameBindingA1 {
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

        checkDuplicates(getIdList(decl))

        // subEnv is used for checking nested procedures.
        var subEnv = env.asInstanceOf[EnvA2A]

        for (cd <- decl.consts) {
            val cType = processExpr(cd.expr, subEnv)
            subEnv = subEnv.addConst(cd.name)
        }

        for (td <- decl.types) {
            checkType(td, subEnv)
            subEnv = subEnv.addType(td.name)
        }

        for (vd <- decl.vars) {
            checkType(vd.varType, subEnv)
        }

        var withVars = subEnv.addVars(getVarNames(decl))
        println("after vardef: " + withVars)

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

        // Inner procedures conflict with parameters.
        checkDuplicates(params ++ procedureNames(proc.decl))

        // Check parameter types.
        for (fp <- proc.params) {
            checkType(fp.pType, env)
        }

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

object IdType extends Enumeration {
    type Val = Value

    val Var, Const, Proc = Value
}

class EnvA2A(parent: EnvA2A,
             defs: Map[String, (Id, IdType.Val)],
             types: Map[String, Id])
        extends EnvBase(parent, Map.empty, types) {
    import IdType._

    def addVars(ids: List[Id]) = {
        val idMap = ids.map((id: Id) => id.text -> (id, Var)).toMap
        new EnvA2A(this, idMap, Map.empty)
    }

    def addConst(id: Id) =
        new EnvA2A(this, Map(id.text -> (id, Const)), Map.empty)

    def addType(id: Id) =
        new EnvA2A(this, Map.empty, Map(id.text -> id))

    def addProcedures(ids: List[Id]) =
        new EnvA2A(this,
            ids.map((id: Id) => id.text -> (id, Proc)).toMap,
            Map.empty)

    def checkProc(id: Id) {
        getDef(id) match {
            case (ref, Proc) => id.ref = ref
            case _ => throw new NameError(id)
        }
    }

    override def checkVar(id: Id, lhs: Boolean) {
        getDef(id) match {
            case (ref, Var) => id.ref = ref
            case (ref, Const) if (!lhs) => id.ref = ref
            case _ =>
                throw new NameError(id)
        }
    }

    protected def getDef(id: Id): (Id, IdType.Val) =
        if (defs.contains(id.text))
            defs(id.text)
        else
            parent.getDef(id)

    def containsDef(name: String): Boolean =
        defs.contains(name) || parent.containsDef(name)

    override def toString = "[" + defs + "] => " + parent
}

object EnvA2A {
    import IdType._

    val Write = Id("Write")
    val WriteLn = Id("WriteLn")
    val Read = Id("Read")

    val preDefs = Map(
        "Write" -> (Write, Proc),
        "WriteLn" -> (WriteLn, Proc),
        "Read" -> (Read, Proc),
        "TRUE" -> (EnvA1.TRUE, Const),
        "FALSE" -> (EnvA1.FALSE, Const)
    )

    val initialEnv =
        new EnvA2A(null, Map.empty, Map.empty) {
            override def getDef(id: Id) =
                if (preDefs.contains(id.text))
                    preDefs(id.text)
                else
                    throw new NameError(id)
            override def containsDef(name: String) = preDefs.contains(name)

            override def getType(name: String) = EnvA1.preTypes.get(name)
            override def toString = "()"
        }
}