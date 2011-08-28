package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._

// Helper for storing arrays with size.
case class OCArray(base: OType, size: gen.Expr) extends OType {
    // This will not be called.
    def assignableFrom(other: OType): Boolean = throw new Exception()
}


object Codegen {
    def generate(module: Module): gen.Module = {
        generateModule(module)
    }

    private def generateModule(module: Module): gen.Module = {
        var procedures = module.decl.procedures.map(generateProcedure)

        val stmt = generateStatements(module.statements)
        if (stmt != gen.Nop()) {
            if (module.decl.procedures.exists(_.name == "main")) {
                throw new Exception(
                    "Procedure main exists and module body is not empty")
            } else {
                procedures =
                    gen.ProcDecl("main",
                        List(gen.Arg("argc", "argv"),
                            gen.Arg("argv", "char **")),
                        List(stmt)) :: procedures
            }
        }

        gen.Module(
                module.name1.text,
                generateDecl(module.decl).toList,
                procedures)
    }

    private def convertField(f: FieldList) =
        f.ids.ids.map(
            (id: Id) => OField(id.text, convertType(f.idType)))

    private def convertType(tv: TypeValue): OType = tv match {
        case Id("INTEGER") =>
            ORef("int")
        case Id("BOOLEAN") =>
            ORef("int")
        case Id(other) =>
            ORef(other)
        case RecordType(fields) =>
            ORecord(fields.flatMap(convertField))
        case ArrayType(size, base) =>
            OCArray(convertType(base), generateExpr(size))
    }

    private def generateDecl(decl: Declarations): Seq[gen.Stmt] = {
        val body = new ArrayBuffer[gen.Stmt]

        for (tDecl <- decl.types) {
            body += gen.Typedef(tDecl.name.text, convertType(tDecl.tValue))
        }

        for (varDecl <- decl.vars; id <- varDecl.vars.ids) {
            body += gen.VarDecl(id.text, convertType(varDecl.varType))
        }

        for (constDecl <- decl.consts) {
            // TODO: convert to C type.
            body += gen.ConstDecl(
                constDecl.name.text,
                "TODO",
                generateExpr(constDecl.expr))
        }

        body
    }

    def generateProcedure(proc: ProcedureDecl) = {
        val body = new ArrayBuffer[gen.Stmt]
        body ++= generateDecl(proc.decl)
        body += generateStatements(proc.body)

        gen.ProcDecl(proc.name.text, getParameters(proc), body.toList)
    }

    private def getParameters(proc: ProcedureDecl) = {
        val params = new ArrayBuffer[gen.Arg]

        for (fp <- proc.params; p <- fp.ids.ids) {
            // TODO: deal with var parameters
            // TODO: use actual type instead of type name.

            // TODO: reinstate
//            params += gen.Arg(p.text, fp.pType.text)
        }
        params.toList
    }

    private def generateStatements(stmt: StatementSequence) =
        if ((stmt eq null) || (stmt eq null))
            gen.Nop()
        else
            gen.Sequence(stmt.stmt.map(generateStatement))

    private def generateStatement(stmt: Statement): gen.Stmt = stmt match {
        case Assignment(lhs, right) =>
            // Assume that the LHS part is an expression that can be used
            // as LHS.
            gen.Assign(generateExpr(lhs), generateExpr(right))
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
            val pre = gen.Assign(gen.Id(id), generateExpr(start))
            val endExpr = generateExpr(end)
            val incr = gen.NumberLit(1)
            val (cond, post) = direction match {
                case To() =>
                    (gen.Binary("<=", gen.Id(id), endExpr), gen.Inc(id, incr))
                case DownTo() =>
                    (gen.Binary(">=", gen.Id(id), endExpr), gen.Dec(id, incr))
            }

            gen.For(pre, cond, post, generateStatements(body))
        // Case statement is translated to series of if statements because
        // C case does not support ranges.
        case _ =>
            println("Unknown: " + stmt)
            null
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
        case RecordAccess(rec, Id(field)) =>
            gen.RecordAccess(generateExpr(rec), field)
        case ArrayAccess(arr, index) =>
            gen.ArrayAccess(generateExpr(arr), generateExpr(index))
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
