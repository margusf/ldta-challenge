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

        val stmt = generateStatements(module.statements)
        println(stmt)

        gen.Module(module.name1.text, null, null, null)
    }

    private def generateDecl(ctx: GenCtx,
                             decl: Declarations): Seq[gen.Statement] = {
        for (proc <- decl.procedures) {
            generateProcedure(ctx, proc)
        }

        val body = new ArrayBuffer[gen.Statement]

        for (varDecl <- decl.vars; id <- varDecl.vars.ids) {
            // TODO: convert to C type.
            body += gen.VarDecl(id.text, varDecl.varType.text)
        }

        for (constDecl <- decl.consts) {
            // TODO: convert to C type.
            body += gen.ConstDecl(constDecl.name.text,
                "TODO",
                generateExpr(constDecl.expr))
        }

        body
    }

    def generateProcedure(ctx: GenCtx, proc: ProcedureDecl) {
        val (params, pTypes) = getParameters(proc)

        val body = new ArrayBuffer[gen.Statement]
        body ++= generateDecl(ctx, proc.decl)
        body ++= generateStatements(proc.body)

        // TODO: add procedure to toplevel.
        ctx.addToplevel(
            gen.ProcDecl(proc.name.text, params, pTypes, body.toList))
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

    private def generateStatements(stmt: StatementSequence): Seq[gen.Statement] = {
        List.empty
    }

    def generateExpr(expr: Expression): gen.Expr =
        null
}

class GenCtx {
    val topLevel = new ArrayBuffer[gen.ProcDecl]

    def addToplevel(proc: gen.ProcDecl) {
        topLevel += proc
    }
}