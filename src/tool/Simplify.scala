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
        doProcedures(module.decl.procedures)
        val ctx = new Ctx
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

    private class Ctx {
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


//    private def doBody(vars: List[VarDef], body: StatementSequence):
//            (List[VarDef], List[Statement]) =
//        if ((body eq null) || (body.stmt eq null))
//            (vars, Nil)
//        else
//            body.stmt.foldRight[(List[VarDef], List[Statement])]((vars, Nil))(doStmt)

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
                case e: Expression =>
                    // TODO: track free variables.
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

    private def doProcedures(procList: List[ProcedureDecl]) {
        for (proc <- procList) {
            doProcedures(proc.decl.procedures)
            // TODO: somehow merge the stuff?
            val ctx = new Ctx
            doStatementSequence(ctx, proc.body)

            topLevel += ProcedureDecl(
                    proc.name,
                    proc.params,
                    Declarations(
                            proc.decl.consts,
                            proc.decl.types,
                            ctx.newVars.toList,
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