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
            Tuple2[List[VarDef], List[Statement]] =
        if ((body eq null) || (body.stmt eq null))
            (vars, Nil)
        else
            body.stmt.foldRight[Tuple2[List[VarDef], List[Statement]]]((vars, Nil))(doStmt)

    private def doStmt(stmt: Statement,
                      old: Tuple2[List[VarDef], List[Statement]]):
            Tuple2[List[VarDef], List[Statement]] = {
        val oldVars = old._1
        val oldBody = old._2
        stmt match {
            case CaseStatement(expr, clauses, elseClause) =>
                // Artificial variable for case expression
                val exprVar = newId
                val exprDef = VarDef(IdentList(List(Id(exprVar))), Id("TODO!"))

                val ifStmt = IfStatement(
                    List(Id(exprVar)),
                    List(StatementSequence(List(ProcedureCall(Id("Foo"), Nil)))),
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