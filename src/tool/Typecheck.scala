package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{CommonNode, SourceMessage}
import collection.mutable.ArrayBuffer


object Typecheck {
    def process(module: Module): List[SourceMessage] = {
        val checker = new Typecheck
        checker.process(module)
        checker.errors.toList
    }
}

class Typecheck {
    val errors = new ArrayBuffer[SourceMessage]

    def process(module: Module) {
        val env = processDeclarations(module.decl, Env.initialEnv)
        processStatements(module.statements, env)
    }

    def processStatements(seq: StatementSequence, env: Env) {
        if (seq ne null) {
            (seq.first :: seq.rest).foreach(processStatement(_, env))
        }
    }

    def processStatement(stm: Statement, env: Env) {
        stm match {
            case Assignment(left, right) =>
                processExpr(left, env)
                processExpr(right, env)
            case ProcedureCall(proc, first, rest) =>
                // TODO: verify that proc is a procedure.
                processExpr(proc, env)
                if (first ne null) {
                    (first :: rest).foreach(processExpr(_, env))
                }
            case IfStatement(cond, ifStmt, elsifCond, elsifStmt, elseStmt) =>
                processExpr(cond, env)
                processStatements(ifStmt, env)
                elsifCond.foreach(processExpr(_, env))
                elsifStmt.foreach(processStatements(_, env))
                processStatements(elseStmt, env)
            case WhileStatement(cond, body) =>
                processExpr(cond, env)
                processStatements(body, env)
        }
    }

    def processDeclarations(decl: Declarations, env: Env): Env = {
        var newEnv = env

        for (cd <- decl.consts) {
            processExpr(cd.expr, newEnv)
            // TODO: separately deal with constants
            newEnv = newEnv.addPrimitive(cd.name, null)
        }

        for (vd <- decl.vars; id <- vd.vars.first :: vd.vars.rest) {
            // TODO: type
            newEnv = newEnv.addPrimitive(id, null)
        }

        for (pd <- decl.procedures) {
            newEnv = processProcedureDecl(pd, newEnv)
        }

        println(newEnv)
        newEnv
    }

    def processProcedureDecl(pd: ProcedureDecl, env: Env) = {
        // TODO: type
        val newEnv = env.addProc(pd.name, null)
        var bodyEnv = newEnv
        for (fp <- pd.firstParam :: pd.rest;
                if fp ne null;
                id <- fp.ids.first :: fp.ids.rest) {
            // TODO: type
            bodyEnv = bodyEnv.addPrimitive(id, null)
        }
        bodyEnv = processDeclarations(pd.decl, bodyEnv)

        processStatements(pd.body, bodyEnv)

        newEnv
    }

    def processExpr(expr: Expression, env: Env) {
        expr match {
            case Id(name) =>
                env.get(name) match {
                    case Some((node, _)) =>
                        expr.asInstanceOf[Id].ref = node
                    case None =>
                        addError("Undefined identifier: " + name, expr)
                }
            case Binary(_, left, right) =>
                processExpr(left, env)
                processExpr(right, env)
            case Unary(_, arg) =>
                processExpr(arg, env)
            case NumberLit(_) =>
                ()
            case ArrayAccess(array, index) =>
                processExpr(array, env)
                processExpr(index, env)
            case RecordAccess(record, field) =>
                processExpr(record, env)
                // TODO: check field against type.
            case _ =>
                throw new IllegalArgumentException(expr.toString)
        }
    }

    def addError(message: String, location: CommonNode) {
        errors += new SourceMessage(message, SourceMessage.Error, location)
    }
}