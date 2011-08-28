package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ee.cyber.simplicitas.{SourceLocation, CommonNode, SourceMessage}
import ast._

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
            seq.stmt.foreach(processStatement(_, env))
        }
    }

    def checkBoolean(expr: Expression, env: Env) {
        val exprType = processExpr(expr, env)
        checkType(Types.bool, exprType, expr)
    }

    def checkInteger(expr: Expression, env: Env) {
        val exprType = processExpr(expr, env)
        checkType(Types.int, exprType, expr)
    }

    def processStatement(stm: Statement, env: Env) {
        stm match {
            case Assignment(left, right) =>
                val leftType = processExpr(left, env)
                val rightType = processExpr(right, env)
                checkType(leftType, rightType, right)
            case ProcedureCall(proc, args) =>
                val procType = processExpr(proc, env)
                procType match {
                    case OProc(formals) =>
                        val argTypes = args.map(processExpr(_, env))
                        if (args.size != formals.size) {
                            addError("Invalid number of parameters: expected " +
                                formals.size + ", but got " + args.size,
                                proc)
                        }
                        for ((ft, (at, al)) <-
                                formals.zip(argTypes.zip(args))) {
                            checkType(ft, at, al)
                        }
                    case OInvalid() =>
                        // Invalid type was reported elsewhere.
                        ()
                    case _ =>
                        addError(proc.text + " is not a procedure", proc)
                }
            case IfStatement(cond, ifStmt, elseStmt) =>
                cond.foreach(checkBoolean(_, env))
                ifStmt.foreach(processStatements(_, env))
                processStatements(elseStmt, env)
            case WhileStatement(cond, body) =>
                checkBoolean(cond, env)
                processStatements(body, env)
            case ForStatement(varName, start, direction, end, body) =>
                checkInteger(varName, env)
                checkInteger(start, env)
                checkInteger(end, env)
                processStatements(body, env)
            case CaseStatement(expr, clauses, elseClause) =>
                checkInteger(expr, env)
                for (clause <- clauses) {
                    processStatements(clause.stmt, env)
                }
                processStatements(elseClause, env)
            case _ =>
                ()
        }
    }

    def processDeclarations(decl: Declarations, env: Env): Env = {
        var newEnv = env

        for (td <- decl.types) {
            newEnv = newEnv.addType(td.name.text, typeValue(td.tValue, newEnv))
        }

        for (cd <- decl.consts) {
            val cType = processExpr(cd.expr, newEnv)
            newEnv = newEnv.addPrimitive(cd.name, cType)
        }

        for (vd <- decl.vars; id <- vd.vars.ids) {
            newEnv = newEnv.addPrimitive(id, typeValue(vd.varType, newEnv))
        }

        for (pd <- decl.procedures) {
            newEnv = processProcedureDecl(pd, newEnv)
        }

        newEnv
    }

    def doField(env: Env)(f: FieldList) =
        f.ids.ids.map(
            (id: Id) => OField(id.text, typeValue(f.idType, env)))

    // Convert parsed TypeValue to OType.
    def typeValue(tv: TypeValue, env: Env): OType = tv match {
        case id @ Id(_) =>
            getType(id, env)
        case RecordType(fields) =>
            ORecord(fields.flatMap(doField(env)))
        case ArrayType(expr, base) =>
            OArray(typeValue(base, env))
    }

    def processProcedureDecl(pd: ProcedureDecl, env: Env) = {
        val newEnv = env.addProc(pd.name, null)
        var bodyEnv = newEnv
        val paramTypes = new ArrayBuffer[OType]

        for (fp <- pd.params;
             if fp ne null;
             id <- fp.ids.ids) {
            val paramType = typeValue(fp.pType, bodyEnv)
            bodyEnv = bodyEnv.addPrimitive(id, paramType)
            paramTypes += paramType
        }
        bodyEnv = processDeclarations(pd.decl, bodyEnv)

        processStatements(pd.body, bodyEnv)

        newEnv.addProc(pd.name, paramTypes)
    }

    /** Reads type from environment. */
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

        val retType = expr match {
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
            case ArrayAccess(array, index) =>
                checkInteger(index, env)
                processExpr(array, env) match {
                    case OArray(base) =>
                        base
                    case err =>
                        println("notarray: " + array + "   " + err)
                        addError("Indexed item is not array", array)
                        Types.invalid
                }
            case RecordAccess(record, field) =>
                val recType = processExpr(record, env)
                recType match {
                    case ORecord(fields) =>
                        fields.find(_.name == field.text) match {
                            case Some(f) =>
                                f.fType
                            case _ =>
                                println("fieldnotfound: " + record + "   " + recType)
                                addError("Record does not contain field " + field.text, field)
                                Types.invalid
                        }

                    case _ =>
                        addError("Not a record", expr)
                        Types.invalid
                }
            case _ =>
                throw new IllegalArgumentException(expr.toString)
        }
        expr.exprType = retType
        retType
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