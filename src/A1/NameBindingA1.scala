package ee.cyber.simplicitas.oberonexample

import ast._
import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.{CommonNode, SourceLocation, SourceMessage}

object NameBindingA1 extends NameBindingA1

class NameBindingA1 {
    protected def initialEnv: EnvBase = EnvA1.initialEnv

    def process(module: Module): Option[SourceMessage] = {
        try {
            if (module.name1 != module.name2) {
                throw NameError(module.name2)
            }

            val env = processDeclarations(module.decl, initialEnv)
            processStatements(module.statements, env)

            None
        } catch {
            case NameError(id: Id) =>
                Some(new SourceMessage(
                    "Invalid identifier: " + id.text, SourceMessage.Error, id))
        }
    }

    def processStatements(seq: StatementSequence, env: EnvBase) {
        if (seq ne null) {
            seq.stmt.foreach(processStatement(_, env))
        }
    }

    def processStatement(stm: Statement, env: EnvBase) {
        stm match {
            case Assignment(lhs @ Id(_), right) =>
                env.checkVar(lhs, true)
                processExpr(right, env)
            case IfStatement(cond, ifStmt, elseStmt) =>
                cond.foreach(processExpr(_, env))
                ifStmt.foreach(processStatements(_, env))
                processStatements(elseStmt, env)
            case WhileStatement(cond, body) =>
                processExpr(cond, env)
                processStatements(body, env)
            case ForStatement(varName, start, end, step, body) =>
                processExpr(varName, env)
                processExpr(start, env)
                processExpr(end, env)
                if (step ne null)
                    processExpr(step, env)
                processStatements(body, env)
            case CaseStatement(expr, clauses, elseClause) =>
                processExpr(expr, env)
                for (clause <- clauses) {
                    processStatements(clause.stmt, env)
                }
                processStatements(elseClause, env)
            case _ =>
                ()
        }
    }

    def processDeclarations(decl: Declarations, env: EnvBase): EnvBase = {
        var newEnv = env

        checkDuplicates(getIdList(decl))

        for (td <- decl.types) {
            checkType(td, newEnv)
            newEnv = newEnv.addType(td.name)
        }

        for (cd <- decl.consts) {
            val cType = processExpr(cd.expr, newEnv)
            newEnv = newEnv.addConst(cd.name)
        }

        for (vd <- decl.vars) {
            checkType(vd.varType, newEnv)
        }
        newEnv = newEnv.addVars(getVarNames(decl))

        newEnv
    }

    protected def getVarNames(decl: Declarations) =
        for (vd <- decl.vars; id <- vd.vars.ids)
            yield id

    protected def getIdList(decl: Declarations) = {
        val typeNames = decl.types.map(_.name)
        val constNames = decl.consts.map(_.name)
        val varNames = getVarNames(decl)

        typeNames ++ constNames ++ varNames
    }

    def checkType(td: TypeDef, env: EnvBase) {
        checkType(td.tValue, env)
    }

    def checkType(tv: TypeValue, env: EnvBase) {
        tv match {
            case id @ Id(name) =>
                env.checkType(id)
        }
    }

    def checkDuplicates(ids: List[Id]) {
        val checked = collection.mutable.Set[String]()

        for (id <- ids) {
            if (checked(id.text)) {
                throw NameError(id)
            } else {
                checked += id.text
            }
        }
    }

    def processExpr(expr: Expression, env: EnvBase) {
        expr match {
            case id @ Id(name) =>
                env.checkVar(id, false)
            case Binary(op, left, right) =>
                processExpr(left, env)
                processExpr(right, env)
            case Unary(op, arg) =>
                processExpr(arg, env)
            case NumberLit(_) =>
                ()
            case _ =>
                throw new IllegalArgumentException(expr.toString)
        }
    }
}

case class NameError(id: Id) extends Exception

class EnvA1(parent: EnvA1,
          defs: Map[String, (Id, Boolean)],
          types: Map[String, Id]) extends EnvBase(parent, defs, types) {
    def addVars(ids: List[Id]) = {
        val idMap = ids.map((id: Id) => id.text -> (id, true)).toMap
        new EnvA1(this, idMap, Map.empty)
    }

    def addConst(id: Id) =
        new EnvA1(this, Map(id.text -> (id, false)), Map.empty)

    def addType(id: Id) =
        new EnvA1(this, Map.empty, Map(id.text -> id))
}

abstract class EnvBase(parent: EnvBase,
                       defs: Map[String, (Id, Boolean)],
                       types: Map[String, Id]) {
    def addVars(ids: List[Id]): EnvBase

    def addConst(id: Id): EnvBase

    def addType(id: Id): EnvBase

    def checkVar(name: Id, lhs: Boolean) {
        get(name.text) match {
            case Some((_, isAssignable)) if (!lhs || isAssignable) =>
                None
            case _ =>
                throw new NameError(name)
        }
    }

    def checkType(id: Id) {
        getType(id.text) match {
            case Some(_) =>
                ()
            case None =>
                throw new NameError(id)
        }
    }

    protected def get(name: String): Option[(Id, Boolean)] =
        if (defs.contains(name))
            Some(defs(name))
        else
            parent.get(name)
//
//    def getFun(name: String): Option[OFunc] =
//        get(name) match {
//            case Some((_, x: OFunc)) => Some(x)
//            case Some(x) => None
//            case None => None
//        }
//
    protected def getType(name: String): Option[Id] =
        if (types.contains(name))
            Some(types(name))
        else
            parent.getType(name)

    override def toString = defs.toString + " ==> " + parent
}

object EnvA1 {
//    import Types._

//    def proc(name: String, params: OType*) =
//        (name, (null, OProc(params)))
//
//    def fun(name: String, ret: OType, params: OType*) =
//        (name, (null, OFunc(params, ret)))
//
//    val predefs = Map[String, Tuple2[CommonNode, OType]](
//        proc("Write", any),
//        proc("WriteLn"),
//        proc("Read", any),
//
//        fun("+", int, int, int),
//        fun("-", int, int, int),
//        fun("*", int, int, int),
//        fun("DIV", int, int, int),
//        fun("MOD", int, int, int),
//
//        fun("<", bool, int, int),
//        fun(">", bool, int, int),
//        fun("<=", bool, int, int),
//        fun(">=", bool, int, int),
//        fun("=", bool, int, int),
//        fun("#", bool, int, int),
//
//        fun("&", bool, bool, bool),
//        fun("OR", bool, bool, bool)
//    )
//

    val predefs = Map[String, (Id, Boolean)](
        "TRUE" -> (Id("TRUE"), false),
        "FALSE" -> (Id("FALSE"), false)
    )

    val preTypes = Map[String, Id](
        "INTEGER" -> Id("INTEGER"),
        "BOOLEAN" -> Id("BOOLEAN")
    )

    val initialEnv =
        new EnvA1(null, Map.empty, Map.empty) {
            override def get(name: String) = predefs.get(name)
            override def getType(name: String) = preTypes.get(name)
            override def toString = "()"
        }
}
