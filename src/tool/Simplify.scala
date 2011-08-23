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
        val (newVars, newBody) =
            doBody(module.decl.vars, module.statements)
        Module(module.name1,
            Declarations(
                module.decl.consts,
                module.decl.types,
                newVars,
                topLevel.toList),
            StatementSequence(newBody),
            module.name2)
    }

    private def doBody(vars: List[VarDef], body: StatementSequence):
            (List[VarDef], List[Statement]) =
        if ((body eq null) || (body.stmt eq null))
            (vars, Nil)
        else
            body.stmt.foldRight[(List[VarDef], List[Statement])]((vars, Nil))(doStmt)

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

    private def doStmt(stmt: Statement,
                      old: (List[VarDef], List[Statement])):
            (List[VarDef], List[Statement]) = {
        val oldVars = old._1
        val oldBody = old._2
        stmt match {
            case CaseStatement(expr, clauses, elseClause) =>
                // Artificial variable for case expression
                val exprVar = newId
                val exprDef = VarDef(IdentList(List(Id(exprVar))), Id("TODO!"))

                val ifClauses = clauses.map(caseClause(exprVar))
                val ifConds = ifClauses.map(_._1)
                val ifStatements = ifClauses.map(_._2)

                val ifStmt = IfStatement(
                    ifConds,
                    ifStatements,
                    elseClause)
                (exprDef :: oldVars,
                        Assignment(Id(exprVar), expr) :: ifStmt :: oldBody)
            case _ =>
                (oldVars, stmt :: oldBody)
        }
    }

    private def doProcedures(procList: List[ProcedureDecl]) {
        for (proc <- procList) {
            doProcedures(proc.decl.procedures)
            val (newVars, newBody) =
                    doBody(proc.decl.vars, proc.body)

            topLevel += ProcedureDecl(
                    proc.name,
                    proc.params,
                    Declarations(
                            proc.decl.consts,
                            proc.decl.types,
                            newVars,
                            Nil),
                    StatementSequence(newBody),
                    proc.name2)
        }
    }

    private def newId = {
        currentId += 1
        "gen_" + currentId
    }
}