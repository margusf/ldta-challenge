package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, LiteralNode}

// Simplifies the Oberon program
// * Brings inner procedures to top level
// * Replaces case statements with series of if statements
object Simplify {
    def simplify(module: Module) {
        val simpl = new Simplify(module)
        simpl()
    }
}

class Simplify(module: Module) {
    // For generating synthetic variable names.
    var currentId = 0

    type VarList = ArrayBuffer[Id]

    private def varDefs(lst: VarList) =
        lst.map(
            (id: Id) =>
                VarDef(IdentList(List(id)), Id("INTEGER"))
        ).toList

    def apply() {
        module.decl.procedures.foreach(doProcedure)

        val newVars = new VarList()

        if (module.statements ne null) {
            module.statements.walkTree(processChild(newVars))
        }

        module.decl.vars ++= varDefs(newVars)
    }

    private def doStatementSequence(varList: VarList, stmt: StatementSequence) {
        if ((stmt ne null) && (stmt.stmt ne null)) {
            stmt.stmt =
                    stmt.stmt.foldRight[List[Statement]](Nil)(doStmt(varList))
        }
    }

    private def caseClause(id: Id)(clause: CaseClause) = {
        def doConst(c: CaseConstant) =
            if (c.end ne null)
                Binary(BinaryOp.And,
                    Binary(BinaryOp.GreaterEqual, newId(id), c.begin),
                    Binary(BinaryOp.LessEqual, newId(id), c.end))
            else
                Binary(BinaryOp.Equals, newId(id), c.begin)

        // List of comparison operators
        val exprList = clause.items.map(doConst)
        // Or the operators together.
        val expr = exprList.reduceLeft(Binary(BinaryOp.Or, _, _))

        (expr, clause.stmt)
    }

    private def doStmt(varList: VarList)(stmt: Statement, old: List[Statement]):
            List[Statement] =
        stmt match {
            case CaseStatement(expr, clauses, elseClause) =>
                // Artificial variable for case expression
                val exprVar = Id(newId)
                exprVar.exprType = Types.int
                exprVar.parent = expr.parent

                val ifClauses = clauses.map(caseClause(exprVar))
                val ifConds = ifClauses.map(_._1)
                val ifStatements = ifClauses.map(_._2)

                val ifStmt = IfStatement(
                    ifConds,
                    ifStatements,
                    elseClause)
                varList += exprVar

                Assignment(newId(exprVar), expr) :: ifStmt :: old
            case _ =>
                // No direct transformation needed. The children were already
                // transformed.
                stmt :: old
        }

    private def processChild(varList: VarList)(child: CommonNode) {
        child match {
            case s: StatementSequence =>
                doStatementSequence(varList, s)
            case _ =>
                // Do nothing
        }
    }

    private def doProcedure(proc: ProcedureDecl) {
        proc.decl.procedures.foreach(doProcedure)

        val newVars = new VarList()
        if (proc.body ne null) {
            proc.body.walkTree(processChild(newVars))
        }

        proc.decl.vars ++= varDefs(newVars)
    }

    private def newId = {
        currentId += 1
        "gen_" + currentId
    }

    /** Makes new Id that references the old Id. */
    private def newId(old: Id) = {
        val ret = Id(old.text)
        ret.ref = old.ref
        ret.byRef = old.byRef
        ret
    }
}