package ee.cyber.simplicitas.oberonexample

import ast._
import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.{CommonNode, SourceLocation, SourceMessage}

object NameBindingA1 {
    def process(module: Module): Option[SourceMessage] = {
        try {
            val env = processDeclarations(module.decl, Env.initialEnv)
            processStatements(module.statements, env)

            None
        } catch {
            case NameError(id: Id) =>
                Some(new SourceMessage(
                    "Invalid identifier: " + id.text, SourceMessage.Error, id))
        }
    }

    def processStatements(seq: StatementSequence, env: Env) {
        if (seq ne null) {
            seq.stmt.foreach(processStatement(_, env))
        }
    }

    def processStatement(stm: Statement, env: Env) {
        stm match {
            case Assignment(lhs @ Id(_), right) =>
                env.check(lhs, false)
                processExpr(right, env)
            case ProcedureCall(proc, args) =>
                // TODO: not supported
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

    def processDeclarations(decl: Declarations, env: Env): Env = {
        // TODO: check types and duplicate names in same scope.
        var newEnv = env

        for (td <- decl.types) {
            newEnv = newEnv.addType(td.name)
        }

        for (cd <- decl.consts) {
            val cType = processExpr(cd.expr, newEnv)
            newEnv = newEnv.addConst(cd.name)
        }

        for (vd <- decl.vars; id <- vd.vars.ids) {
            newEnv = newEnv.addPrimitive(id)
        }

        newEnv
    }

    def processExpr(expr: Expression, env: Env) {
        expr match {
            case id @ Id(name) =>
                env.check(id, true)
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

class Env(parent: Env,
          defs: Map[String, (Id, Boolean)],
          types: Map[String, Id]) {
    def addPrimitive(id: Id) =
        new Env(this, Map(id.text -> (id, true)), Map.empty)

    def addConst(id: Id) =
        new Env(this, Map(id.text -> (id, false)), Map.empty)

    def addType(id: Id) =
        new Env(this, Map.empty, Map(id.text -> id))

    def check(name: Id, lhs: Boolean) {
        get(name.text) match {
            case Some((_, isAssignable)) if (lhs && isAssignable) =>
                None
            case _ =>
                throw new NameError(name)
        }
    }

    def get(name: String): Option[(Id, Boolean)] =
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
//    def getType(name: String): Option[OType] =
//        if (types.contains(name))
//            Some(types(name))
//        else
//            parent.getType(name)
//
//
    override def toString = defs.toString + " ==> " + parent
}

object Env {
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
//    val preTypes = Map[String, OType](
//        "INTEGER" -> Types.int,
//        "BOOLEAN" -> Types.bool
//    )

    def initialEnv =
        new Env(null, Map.empty, Map.empty) {
            override def get(name: String) = None
//            override def getType(name: String) = None
            override def toString = "()"
        }
}
