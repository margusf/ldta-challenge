package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.{SourceLocation, CommonNode, SourceMessage}


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
            newEnv = newEnv.addPrimitive(id, getType(vd.varType))
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
            val paramType = getType(fp.pType)
            bodyEnv = bodyEnv.addPrimitive(id, paramType)
        }
        bodyEnv = processDeclarations(pd.decl, bodyEnv)

        processStatements(pd.body, bodyEnv)

        newEnv
    }

    def getType(id: Id) = id match {
        case Id("BOOLEAN") => Types.bool
        case Id("INTEGER") => Types.int
        case Id(other) =>
            addError("Invalid type: " + other, id)
            Types.any
    }

    def processExpr(expr: Expression, env: Env): OType = {
        def processFunCall(op: String, args: List[Expression]) = {
            env.getFun(op) match {
                case Some(OFunc(aTypes, rType)) =>
                    // TODO: check arity
                    for ((a, t) <- args.zip(aTypes)) {
                        val aType = processExpr(a, env)
                        checkType(t, aType, a)
                    }

                    rType
                case None =>
                    addError("Unknown function: " + op, expr)
                    Types.invalid
            }
        }

        expr match {
            case Id(name) =>
                env.get(name) match {
                    case Some((node, oType)) =>
                        expr.asInstanceOf[Id].ref = node
                        oType
                    case None =>
                        addError("Undefined identifier: " + name, expr)
                        Types.any
                }
            case Binary(op, left, right) =>
                processFunCall(op.toString, List(left, right))
            case Unary(op, arg) =>
                processFunCall(op.toString, List(arg))
            case NumberLit(_) =>
                Types.int
//            case ArrayAccess(array, index) =>
//                processExpr(array, env)
//                processExpr(index, env)
//            case RecordAccess(record, field) =>
//                processExpr(record, env)
//                // TODO: check field against type.
            case _ =>
                throw new IllegalArgumentException(expr.toString)
        }
    }

    def checkType(expected: OType, received: OType, loc: SourceLocation) {
        if (!expected.assignableFrom(received)) {
            addError("Type error: expected " + expected + ", but got " + received,
                loc)
        }
    }

    def addError(message: String, location: SourceLocation) {
        errors += new SourceMessage(message, SourceMessage.Error, location)
    }
}