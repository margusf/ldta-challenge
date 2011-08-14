package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._

object Codegen {
    def generate(module: Module): gen.Module = {
        val ctx = new GenCtx
        val ret = generateModule(ctx, module)

        println(ctx.topLevel)
        ret
    }

    private def generateModule(ctx: GenCtx, module: Module): gen.Module = {
        println(generateDecl(ctx, module.decl))

        val stmt = generateStatements(module.statements)
        println(stmt)

        gen.Module(module.name1.text, Nil, Nil, ctx.topLevel.toList)
    }

    private def generateDecl(ctx: GenCtx,
                             decl: Declarations): Seq[gen.Stmt] = {
        for (proc <- decl.procedures) {
            generateProcedure(ctx, proc)
        }

        val body = new ArrayBuffer[gen.Stmt]

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
        val body = new ArrayBuffer[gen.Stmt]
        body ++= generateDecl(ctx, proc.decl)
        body += generateStatements(proc.body)

        ctx.addToplevel(
            gen.ProcDecl(proc.name.text, getParameters(proc), body.toList))
    }

    private def getParameters(proc: ProcedureDecl) = {
        val params = new ArrayBuffer[gen.Arg]

        for (fp <- proc.params; p <- fp.ids.ids) {
            // TODO: deal with var parameters
            // TODO: use actual type instead of type name.
            params += gen.Arg(p.text, fp.pType.text)
        }
        params.toList
    }

    private def generateStatements(stmt: StatementSequence) =
        if ((stmt eq null) || (stmt eq null))
            gen.Nop()
        else
            gen.Sequence(stmt.stmt.map(generateStatement))

    private def generateStatement(stmt: Statement): gen.Stmt = stmt match {
        case Assignment(Id(id), right) =>
            gen.Assign(id, generateExpr(right))
        case ProcedureCall(Id(name), args) =>
            // TODO: add additional arguments corresponding to
            // variables defined in outer scope.
            gen.FunCall(name,
                if (args eq null)
                    List.empty
                else
                    args.map(generateExpr))
        case IfStatement(cond, ifStmt, elseStmt) => {
            def loop(c: List[Expression],
                     i: List[StatementSequence]): gen.Stmt =
                (c, i) match {
                    case (ch :: ct, ih :: it) =>
                        gen.If(generateExpr(ch), generateStatements(ih),
                            loop(ct, it))
                    case _ =>
                        generateStatements(elseStmt)
                }

            loop(cond, ifStmt)
        }

//    | WhileStatement
//    | ForStatement
//    | CaseStatement
        case _ => null
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