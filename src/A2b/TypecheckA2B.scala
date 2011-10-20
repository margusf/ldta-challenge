package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, SourceLocation, SourceMessage}

object TypecheckA2B {
    def process(module: Module): Option[SourceMessage] = {
        try {
            val checker = new TypecheckA2B
            checker.process(module)
            None
        } catch {
            case TypeError(loc, msg) =>
                Some(new SourceMessage(
                    msg, SourceMessage.Error, loc))
        }
    }
}

case class TypeError(location: SourceLocation, msg: String) extends Exception

class TypecheckA2B {
    def process(module: Module) {
        val env = processDeclarations(module.decl, Env.initialEnv)
        processStatements(module.statements)
    }

    private def processStatements(seq: StatementSequence) {
        if (seq ne null) {
            seq.stmt.foreach(processStatement)
        }
    }

    private def checkBoolean(expr: Expression) {
        val exprType = processExpr(expr)
        checkType(Types.bool, exprType, expr)
    }

    private def checkInteger(expr: Expression) {
        val exprType = processExpr(expr)
        checkType(Types.int, exprType, expr)
    }

    private def processStatement(stm: Statement) {
        stm match {
            case Assignment(left, right) =>
                val leftType = processExpr(left)
                val rightType = processExpr(right)
                checkType(leftType, rightType, right)
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
                    checkInteger(step)
                }
                processStatements(body)
            case CaseStatement(expr, clauses, elseClause) =>
                checkInteger(expr)
                for (clause <- clauses) {
                    processStatements(clause.stmt)
                }
                processStatements(elseClause)
            case _ =>
                ()
        }
    }

    private def processDeclarations(decl: Declarations, env: Env): Env = {
        var newEnv = env

        for (td <- decl.types) {
            newEnv = newEnv.addType(td.name.text, typeValue(td.tValue, newEnv))
        }

        for (cd <- decl.consts) {
            val cType = processExpr(cd.expr)
            cd.name.exprType = cType
        }

        for (vd <- decl.vars; id <- vd.vars.ids) {
            id.exprType = typeValue(vd.varType, newEnv)
        }

        newEnv
    }

    // Convert parsed TypeValue to OType.
    private def typeValue(tv: TypeValue, env: Env): OType = tv match {
        case id @ Id(_) =>
            getType(id, env)
    }

    /** Reads type from environment. */
    private def getType(id: Id, env: Env) = env.getType(id.text) match {
        case Some(t) => t
        case None =>
            throw new TypeError(id, "Invalid type: " + id.text)
    }

    private def processExpr(expr: Expression): OType = {
        def processFunCall(op: String, args: List[Expression]) = {
            Env.operators.get(op) match {
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
            case id @ Id(name) =>
                id.ref.asInstanceOf[Id].exprType.asInstanceOf[OType]
            case Binary(op, left, right) =>
                processFunCall(op.toString, List(left, right))
            case Unary(op, arg) =>
                processFunCall(op.toString, List(arg))
            case NumberLit(_) =>
                Types.int
            case _ =>
                throw new IllegalArgumentException(expr.toString)
        }
        expr.exprType = retType
        retType
    }

    private def checkType(expected: OType, received: OType,
                          loc: SourceLocation) {
        if (!expected.assignableFrom(received)) {
            throw new TypeError(loc,
                "Type error: expected " + expected + ", but got " + received)
        }
    }
}

class Env(parent: Env, types: Map[String, OType]) {
    def addType(name: String, typeVal: OType) =
        new Env(this, Map(name -> typeVal))

    def getType(name: String): Option[OType] =
        if (types.contains(name))
            Some(types(name))
        else
            parent.getType(name)

    override def toString = types.toString + " ==> " + parent
}

object Env {
    import Types._

    def proc(name: String, params: OType*) =
        (name, (null, OProc(params)))

    def fun(name: String, ret: OType, params: OType*) =
        (name -> OFunc(params, ret))

    val predefs = Map[String, Tuple2[CommonNode, OType]](
        proc("Write", any),
        proc("WriteLn"),
        proc("Read", any)
    )

    val operators = Map[String, OType](
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

    def initialEnv =
        new Env(null, preTypes) {
            override def getType(name: String) = preTypes.get(name)
            override def toString = "()"
        }
}
