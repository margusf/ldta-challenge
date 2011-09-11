package ee.cyber.simplicitas.oberonexample

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
    val topLevel = ArrayBuffer[ProcedureDecl]()
    var currentId = 0

    def apply(): Module = {
        val ctx = new Ctx(getIds(module.decl))
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
        def doVarDef(vd: VarDef) =
            for (id <- vd.vars.ids) yield id.text

        val consts = decl.consts.map(_.name.text)
        val vars = decl.vars.flatMap(doVarDef)

        (consts ++ vars).toSet
    }

    private class Ctx(val globals: Set[String]) {
        /** Variables introduced by transformations within statements. */
        val newVars = ArrayBuffer[VarDef]()
        /** Free variables that are used by this procedure. */
        val freeVars = collection.mutable.Set[String]()
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
                case id: Id if (id.exprType.isInstanceOf[ONonData]) =>
                    ctx.freeVars += id.text
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

    private def doProcedures(ctx: Ctx, procList: List[ProcedureDecl]) {
        for (proc <- procList) {
            val subCtx = new Ctx(ctx.globals)
            doProcedures(subCtx, proc.decl.procedures)
            val bodyCtx = new Ctx(ctx.globals)
            doStatementSequence(ctx, proc.body)

            val myVars = getIds(proc.decl)
            val deltaVars = (bodyCtx.freeVars ++ subCtx.freeVars -- myVars).toList
            println("deltaVars(" + proc.name + "): " + deltaVars)

            topLevel += ProcedureDecl(
                    proc.name,
                    proc.params,
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