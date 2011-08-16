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
        val vars =
            if (module.decl ne null)
                module.decl.vars
            else
                Nil
        val (newVars, newBody) = doBody(vars, module.statements)
        Module(module.name1,
            Declarations(
                module.decl.consts,
                module.decl.types,
                newVars,
                module.decl.procedures), // TODO: include new toplevel stuff
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
            Tuple2[List[VarDef], List[Statement]]=
        stmt match {
            case _ =>
                (old._1, stmt :: old._2)
        }

    private def newId = {
        currentId += 1
        "gen_" + currentId
    }
}