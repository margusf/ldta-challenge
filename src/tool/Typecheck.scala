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
        def checkBoolean(expr: Expression, env: Env) {
            val exprType = processExpr(expr, env)
            checkType(Types.bool, exprType, expr)
        }

        stm match {
            case Assignment(left, right) =>
                processExpr(left, env)
                processExpr(right, env)
            case ProcedureCall(proc, first, rest) =>
                val procType = processExpr(proc, env)
                procType match {
                    case OProc(formals) =>
                        val argList =
                            if (first ne null)
                                (first :: rest)
                            else
                                List.empty
                        val argTypes = argList.map(processExpr(_, env))
                        if (argList.size != formals.size) {
                            addError("Invalid number of parameters: expected " +
                                formals.size + ", but got " + argList.size,
                                proc)
                        }
                        for ((ft, (at, al)) <-
                                formals.zip(argTypes.zip(argList))) {
                            checkType(ft, at, al)
                        }
                    case OInvalid() =>
                        // Invalid type was reported elsewhere.
                        ()
                    case _ =>
                        addError(proc.text + " is not a procedure", proc)
                }
            case IfStatement(cond, ifStmt, elsifCond, elsifStmt, elseStmt) =>
                checkBoolean(cond, env)
                processStatements(ifStmt, env)
                elsifCond.foreach(checkBoolean(_, env))
                elsifStmt.foreach(processStatements(_, env))
                processStatements(elseStmt, env)
            case WhileStatement(cond, body) =>
                checkBoolean(cond, env)
                processStatements(body, env)
            case _ =>
                ()
        }
    }

    def processDeclarations(decl: Declarations, env: Env): Env = {
        var newEnv = env

        for (td <- decl.types) {
            newEnv = newEnv.addType(td.name.text, getType(td.tValue, newEnv))
        }

        for (cd <- decl.consts) {
            val cType = processExpr(cd.expr, newEnv)
            newEnv = newEnv.addPrimitive(cd.name, cType)
        }

        for (vd <- decl.vars; id <- vd.vars.first :: vd.vars.rest) {
            newEnv = newEnv.addPrimitive(id, getType(vd.varType, newEnv))
        }

        for (pd <- decl.procedures) {
            newEnv = processProcedureDecl(pd, newEnv)
        }

        newEnv
    }

    def processProcedureDecl(pd: ProcedureDecl, env: Env) = {
        val newEnv = env.addProc(pd.name, null)
        var bodyEnv = newEnv
        val paramTypes = new ArrayBuffer[OType]

        for (fp <- pd.firstParam :: pd.rest;
                if fp ne null;
                id <- fp.ids.first :: fp.ids.rest) {
            val paramType = getType(fp.pType, bodyEnv)
            bodyEnv = bodyEnv.addPrimitive(id, paramType)
            paramTypes += paramType
        }
        bodyEnv = processDeclarations(pd.decl, bodyEnv)

        processStatements(pd.body, bodyEnv)

        newEnv.addProc(pd.name, paramTypes)
    }

    def getType(id: Id, env: Env) = env.getType(id.text) match {
        case Some(t) => t
        case None =>
            addError("Invalid type: " + id.text, id)
            Types.invalid
    }

    def processExpr(expr: Expression, env: Env): OType = {
        def processFunCall(op: String, args: List[Expression]) = {
            env.getFun(op) match {
                case Some(OFunc(aTypes, rType)) =>
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
                        Types.invalid
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
        // Do not report type errors for expressions that are already
        // incorrectly typed.
        if (expected != Types.invalid && received != Types.invalid &&
                !expected.assignableFrom(received)) {
            addError("Type error: expected " + expected + ", but got " + received,
                loc)
        }
    }

    def addError(message: String, location: SourceLocation) {
        errors += new SourceMessage(message, SourceMessage.Error, location)
    }
}