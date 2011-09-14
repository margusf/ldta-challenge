package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.LiteralNode

import collection.mutable.ArrayBuffer
import ast._

// Simplifies the Oberon program
// * Brings inner procedures to top level
// * Replaces case statements with series of if statements
object Simplify {
    def simplify(module: Module): Module = {
        val simpl = new Simplify(module)
        simpl()
    }
}

class Simplify(module: Module) {
    // List of top-level procedures (will be filled when procedure
    // declarations are flattened)
    val topLevel = ArrayBuffer[ProcedureDecl]()
    // For generating synthetic variable names.
    var currentId = 0
    // Mapping from procedure names to list of additional parameters
    // to the procedure
    val extraParams = collection.mutable.Map[Id, List[Id]]()

    def apply(): Module = {
        val ctx = new Ctx(getIds(module.decl), null)
        // As all the top-level variables are visible to procedures,
        // there is no need to actually process the ctx.freeVars
        doProcedures(ctx, module.decl.procedures)
        doStatementSequence(ctx, module.statements)
        Module(module.name1,
            Declarations(
                module.decl.consts,
                module.decl.types,
                module.decl.vars ++ ctx.newVars,
                topLevel.toList),
            module.statements,
            module.name2)
    }

    private def getIds(decl: Declarations) = {
        val consts = decl.consts.map(_.name)
        val vars = decl.vars.flatMap(_.vars.ids)

        (consts ++ vars).toSet
    }

    private def getIds(params: List[FormalParam]) =
        if (params eq null)
            Set.empty
        else
            params.flatMap(_.ids.ids).toSet

    private class Ctx(val globals: Set[Id], val parent: String) {
        /** Variables introduced by transformations within statements. */
        val newVars = ArrayBuffer[VarDef]()
        /** Free variables that are used by this procedure. */
        val freeVars = collection.mutable.Set[Id]()

        def getFullName(lastPart: String) =
            if (parent eq null)
                lastPart
            else
                parent + "_" + lastPart
    }

    private def doStatementSequence(ctx: Ctx, stmt: StatementSequence) {
        if ((stmt ne null) && (stmt.stmt ne null)) {
            stmt.stmt = stmt.stmt.foldRight[List[Statement]](Nil)(doStmt(ctx))
        }
    }

    private def caseClause(id: String)(clause: CaseClause) = {
        def doConst(c: CaseConstant) =
            if (c.end ne null)
                Binary(BinaryOp.And,
                    Binary(BinaryOp.GreaterEqual, Id(id), c.begin),
                    Binary(BinaryOp.LessEqual, Id(id), c.end))
            else
                Binary(BinaryOp.Equals, Id(id), c.begin)

        // List of comparison operators
        val exprList = clause.items.map(doConst)
        // Or the operators together.
        val expr = exprList.reduceLeft(Binary(BinaryOp.Or, _, _))

        (expr, clause.stmt)
    }

    private def doStmt(ctx: Ctx)(stmt: Statement, old: List[Statement]):
            List[Statement] = {
        // Process all the child elements -- statements, expressions.
        for (child <- stmt.children) {
            child match {
                case id: Id if (!id.exprType.isInstanceOf[ONonData]) =>
                    ctx.freeVars += id
                case s: StatementSequence =>
                    doStatementSequence(ctx, s)
                case _ =>
                    // Do nothing
            }
        }

        stmt match {
            case CaseStatement(expr, clauses, elseClause) =>
                // Artificial variable for case expression
                val exprVar = newId
                val exprDef = VarDef(
                    IdentList(List(Id(exprVar))),
                    Id("INTEGER"))

                val ifClauses = clauses.map(caseClause(exprVar))
                val ifConds = ifClauses.map(_._1)
                val ifStatements = ifClauses.map(_._2)

                val ifStmt = IfStatement(
                    ifConds,
                    ifStatements,
                    elseClause)
                ctx.newVars += exprDef
                Assignment(Id(exprVar), expr) :: ifStmt :: old
            case _ =>
                // No direct transformation needed. The children were already
                // transformed.
                stmt :: old
        }
    }

    def getType(id: Id) = {
        id.ref.parent match {
            case ConstantDef(_, _, expr) =>
                expr.exprType match {
                    case OInt() => Id("INTEGER")
                    case OBool() => Id("BOOLEAN")
                    case _ =>
                        println("PARENT: " + id.ref.parent)
                        println("TYPE: " + expr.exprType)
                        throw new Exception("Invalid type: " +
                            expr.exprType)
                }
            case idList @ IdentList(_) =>
                idList.parent match {
                    case VarDef(_, vt) => vt
                    case FormalParam(_, _, pt) => pt
                    case _ => throw new Exception("Invalid parent: " +
                            idList.parent)
                }
            case _ =>
                throw new Exception("Invalid parent: " + id.ref.parent)
        }
    }

    private def doProcedures(ctx: Ctx, procList: List[ProcedureDecl]) {
        for (proc <- procList) {
            // For flattening, change the name, but leave the Id object
            // unchanged.
            proc.name.text = ctx.getFullName(proc.name.text)

            val subCtx = new Ctx(ctx.globals, proc.name.text)
            doProcedures(subCtx, proc.decl.procedures)
            val bodyCtx = new Ctx(ctx.globals, proc.name.text)
            doStatementSequence(bodyCtx, proc.body)

            val myVars = getIds(proc.decl) ++ getIds(proc.params)
            val deltaVars = (bodyCtx.freeVars ++ subCtx.freeVars -- myVars).toList
            println("myvars(" + proc.name + "):" + myVars)
            println("freevars(" + proc.name + "):" + (bodyCtx.freeVars ++ subCtx.freeVars))
            println("deltaVars(" + proc.name + "): " + deltaVars)

            val newParams = deltaVars.map((id: Id) =>
                FormalParam(
                    (if (id.isByRef)
                        LiteralNode("VAR")
                    else
                        null),
                    IdentList(List(id)), // xxx: ideally we should make copy and update refs
                    getType(id)))

            extraParams(proc.name) = deltaVars

            topLevel += ProcedureDecl(
                    proc.name,
                    proc.params ++ newParams,
                    Declarations(
                            proc.decl.consts,
                            proc.decl.types,
                            proc.decl.vars ++ ctx.newVars,
                            Nil),
                    proc.body,
                    proc.name2)
        }
    }

    private def newId = {
        currentId += 1
        "gen_" + currentId
    }
}