/*
 * // Copyright (c) 2010 Cybernetica AS / STACC
 */

package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, SourceLocation, SourceMessage}

object TypecheckA2B extends TypecheckA2B

class TypecheckA2B {
    import ConstantEval._

    def process(module: Module) {
        val env = processDeclarations(module.decl, EnvA2B.initialEnv)
        processStatements(module.statements)
    }

    protected def processStatements(seq: StatementSequence) {
        if (seq ne null) {
            seq.stmt.foreach(processStatement)
        }
    }

    private def checkBoolean(expr: Expression) {
        val exprType = processExpr(expr)
        checkType(Types.bool, exprType, expr)
    }

    protected def checkInteger(expr: Expression) {
        val exprType = processExpr(expr)
        checkType(Types.int, exprType, expr)
    }

    def canBeLhs(expr: Expression) = expr match {
        case id @ Id(_) if id.ref.constVal == None =>
            true
        case _ =>
            false
    }

    protected def processStatement(stm: Statement) {
        stm match {
            case Assignment(left, right) =>
                val leftType = processExpr(left)
                val rightType = processExpr(right)
                checkType(leftType, rightType, right)
                if (!canBeLhs(left)) {
                    throw new TypeError(left, "Cannot be used as LHS: " + left)
                }
            case IfStatement(cond, ifStmt, elseStmt) =>
                cond.foreach(checkBoolean)
                ifStmt.foreach(processStatements)
                processStatements(elseStmt)
            case WhileStatement(cond, body) =>
                checkBoolean(cond)
                processStatements(body)
            case ForStatement(varName, start, end, step, body) =>
                checkInteger(varName)
                checkInteger(start)
                checkInteger(end)
                if (step ne null) {
                    evalConstExpr(step)
                }
                processStatements(body)
            case CaseStatement(expr, clauses, elseClause) =>
                checkInteger(expr)
                for (clause <- clauses) {
                    for (ci <- clause.items) {
                        evalConstExpr(ci.begin)
                        if (ci.end ne null) {
                            evalConstExpr(ci.end)
                        }
                    }
                    processStatements(clause.stmt)
                }
                processStatements(elseClause)
            case _ =>
                ()
        }
    }

    protected def processDeclarations(decl: Declarations,
            env: EnvA2B): EnvA2B = {
        var newEnv = env

        for (cd <- decl.consts) {
            val cv = evalConstExpr(cd.expr)
            cd.name.exprType = Types.int
            cd.name.constVal = Some(cv)
        }

        for (td <- decl.types) {
            newEnv = newEnv.addType(td.name.text, typeValue(td.tValue, newEnv))
        }

        for (vd <- decl.vars; id <- vd.vars.ids) {
            id.exprType = typeValue(vd.varType, newEnv)
        }

        newEnv
    }

    // Convert parsed TypeValue to OType.
    protected def typeValue(tv: TypeValue, env: EnvA2B): OType = tv match {
        case id @ Id(_) =>
            getType(id, env)
    }

    /** Reads type from environment. */
    protected def getType(id: Id, env: EnvA2B) = env.getType(id.text) match {
        case Some(t) => t
        case None =>
            throw new TypeError(id, "Invalid type: " + id.text)
    }

    protected def processExpr(expr: Expression): OType = {
        def processFunCall(op: String, args: List[Expression]) = {
            EnvA2B.operators.get(op) match {
                case Some(OFunc(aTypes, rType)) =>
                    for ((a, t) <- args.zip(aTypes)) {
                        val aType = processExpr(a)
                        checkType(t, aType, a)
                    }

                    rType
                case None =>
                    throw new TypeError(expr, "Unknown function: " + op)
            }
        }

        val retType = expr match {
            case id @ Id(_) =>
                id.ref.exprType.asInstanceOf[OType]
            case Binary(op, left, right) =>
                processFunCall(op.toString, List(left, right))
            case Unary(op, arg) =>
                processFunCall("U" + op.toString, List(arg))
            case NumberLit(_) =>
                Types.int
            case _ =>
                throw new IllegalArgumentException(expr.toString)
        }
        expr.exprType = retType
        retType
    }

    protected def checkType(expected: OType, received: OType,
                          loc: SourceLocation) {
        if (!expected.assignableFrom(received)) {
            throw new TypeError(loc,
                "Type error: expected " + expected + ", but got " + received)
        }
    }
}

class EnvA2B(parent: EnvA2B, types: Map[String, OType]) {
    def addType(name: String, typeVal: OType) =
        new EnvA2B(this, Map(name -> typeVal))

    def getType(name: String): Option[OType] =
        if (types.contains(name))
            Some(types(name))
        else
            parent.getType(name)

    override def toString = types.toString + " ==> " + parent
}

object EnvA2B {
    import Types._

    def proc(name: String, params: OType*) =
        (name, (null, OProc(params.map((_, ProcParamType.byValue)))))

    def fun(name: String, ret: OType, params: OType*) =
        (name -> OFunc(params, ret))

    val operators = Map[String, OType](
        fun("U~", bool, bool),
        fun("U-", int, int),
        fun("U+", int, int),

        fun("+", int, int, int),
        fun("-", int, int, int),
        fun("*", int, int, int),
        fun("DIV", int, int, int),
        fun("MOD", int, int, int),

        fun("<", bool, int, int),
        fun(">", bool, int, int),
        fun("<=", bool, int, int),
        fun(">=", bool, int, int),
        fun("=", bool, int, int),
        fun("#", bool, int, int),

        fun("&", bool, bool, bool),
        fun("OR", bool, bool, bool)
    )

    val preTypes = Map[String, OType](
        "INTEGER" -> Types.int,
        "BOOLEAN" -> Types.bool
    )

    EnvA1.TRUE.exprType = bool
    EnvA1.FALSE.exprType = bool

    def initialEnv =
        new EnvA2B(null, preTypes) {
            override def getType(name: String) = preTypes.get(name)
            override def toString = "()"
        }
}
