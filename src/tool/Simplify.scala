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
        val (newDecl, newBody) = doBody(module.decl, module.statements)
        Module(module.name1, newDecl, newBody, module.name2)
    }

    private def doBody(decl: Declarations, body: StatementSequence) = {
        (decl, body)
    }

    private def newId = {
        currentId += 1
        "gen_" + currentId
    }
}