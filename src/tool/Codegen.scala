package ee.cyber.simplicitas.oberonexample

import gen.Gen
import collection.mutable.ArrayBuffer

object Codegen {
    def generate(module: Module) {
        val ctx = new GenCtx
        generateModule(ctx, module)

        println(ctx.topLevel)
    }

    private def generateModule(ctx: GenCtx, module: Module) {
        println(generateDecl(ctx, module.decl))

        val stmt = generateStatement(module.statements)
        println(stmt)

        gen.Module(module.name1.text, null, null, null)
    }

    private def generateDecl(ctx: GenCtx, decl: Declarations): Gen = {
        // TODO: create list of vars.
        for (proc <- decl.procedures) {
            val (params, pTypes) = getParameters(proc)

            // TODO: generate procedure.
            // TODO: add procedure to toplevel.
            ctx.addToplevel(
                gen.ProcDecl(proc.name.text, params, pTypes, List.empty))
        }

        null
    }

    private def getParameters(proc: ProcedureDecl) = {
        val params = new ArrayBuffer[String]
        val pTypes = new ArrayBuffer[String]

        for (fp <- proc.params; p <- fp.ids.ids) {
            // TODO: deal with var parameters
            params += p.text
            // TODO: use actual type instead of type name.
            pTypes += fp.pType.text
        }
        (params.toList, pTypes.toList)
    }

    private def generateStatement(stmt: StatementSequence): Gen = {
        null
    }
}

class GenCtx {
    val topLevel = new ArrayBuffer[gen.ProcDecl]

    def addToplevel(proc: gen.ProcDecl) {
        topLevel += proc
    }
}