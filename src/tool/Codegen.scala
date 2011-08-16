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
        if (stmt != gen.Nop()) {
            if (ctx.topLevel.exists(_.name == "main")) {
                throw new Exception(
                    "Procedure main exists and module body is not empty")
            } else {
                ctx.addToplevel(gen.ProcDecl("main",
                    List(gen.Arg("int", "argc"), gen.Arg("argv", "char **")),
                    List(stmt)))
            }
        }

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
        case WhileStatement(cond, body) =>
            gen.While(generateExpr(cond), generateStatements(body))
        case ForStatement(Id(id), start, direction, end, body) =>
            val pre = gen.Assign(id, generateExpr(start))
            val endExpr = generateExpr(end)
            val incr = gen.NumberLit(1)
            val (cond, post) = direction match {
                case To() =>
                    (gen.Binary("<=", gen.Id(id), endExpr), gen.Inc(id, incr))
                case DownTo() =>
                    (gen.Binary(">=", gen.Id(id), endExpr), gen.Dec(id, incr))
            }

            gen.For(pre, cond, post, generateStatements(body))

//    | CaseStatement
        case _ => null
    }

    def generateExpr(expr: Expression): gen.Expr = expr match {
        case Id(name) =>
            gen.Id(name)
        case Binary(op, left, right) =>
            gen.Binary(cBinOps(op), generateExpr(left), generateExpr(right))
        case Unary(op, arg) =>
            gen.Unary(cUnOps(op), generateExpr(arg))
        case NumberLit(value) =>
            gen.NumberLit(value.toInt)
        case _ =>
            throw new IllegalArgumentException(expr.toString)
    }

    val cBinOps = Map(
        BinaryOp.Plus -> "+",
        BinaryOp.Minus -> "-",
        BinaryOp.Times -> "*",
        BinaryOp.Div -> "/",
        BinaryOp.Mod -> "%",
        BinaryOp.LessThan -> "<",
        BinaryOp.LessEqual -> "<=",
        BinaryOp.GreaterThan -> ">",
        BinaryOp.GreaterEqual -> ">=",
        BinaryOp.Equals -> "==",
        BinaryOp.NotEquals -> "!=",
        BinaryOp.And -> "&&",
        BinaryOp.Or -> "||"
    )

    val cUnOps = Map(
        UnaryOp.Pos -> "",
        UnaryOp.Not -> "!",
        UnaryOp.Neg -> "-"
    )
}

class GenCtx {
    val topLevel = new ArrayBuffer[gen.ProcDecl]

    def addToplevel(proc: gen.ProcDecl) {
        topLevel += proc
    }
}