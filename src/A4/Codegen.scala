package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._

// Helper for storing arrays with size.
case class OCArray(base: OType, size: gen.Expr) extends OType {
    // This will not be called.
    def assignableFrom(other: OType): Boolean = throw new Exception()
}


object Codegen {
    import ConstantEval._

    def generate(module: Module): gen.Module = {
        generateModule(module)
    }

    private def generateModule(module: Module): gen.Module = {
        var procedures = module.decl.procedures.map(generateProcedure)

        gen.Module(
                module.name1.text,
                generateDecl(module.decl).toList,
                procedures,
                generateStatements(module.statements))
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
            val varName =
                if (p.isByRef)
                    "*" + p.text
                else
                    p.text
            params += gen.Arg(varName, convertType(fp.pType))
        }
        params.toList
    }

    private def generateStatements(stmt: StatementSequence) =
        if ((stmt eq null) || (stmt eq null))
            gen.Nop()
        else
            gen.Sequence(stmt.stmt.map(generateStatement))

    private def generateProcArg(x: (Expression, (OType, ProcParamType.Type))) =
        x match {
            case (expr, (_, ProcParamType.byValue)) =>
                generateExpr(expr)
            case (expr, (_, ProcParamType.byRef)) =>
                gen.AddressOf(generateExpr(expr))
        }

    private def generateStatement(stmt: Statement): gen.Stmt = stmt match {
        case Assignment(lhs, right) =>
            // Assume that the LHS part is an expression that can be used
            // as LHS.
            gen.Assign(generateExpr(lhs), generateExpr(right))
        case ProcedureCall(nameId @ Id(name), args) =>
            nameId.ref.exprType.asInstanceOf[OType] match {
                case OProc(argTypes) =>
                    val genArgs =
                        if ((args eq null) || args.isEmpty)
                            Nil
                        else
                            args.zip(argTypes).map(generateProcArg)
                    gen.FunCall(name, genArgs)
            }
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
        case ForStatement(id @ Id(varName), start, end, step, body) =>
            val pre = gen.Assign(
                gen.Id(varName, id.isByRef),
                generateExpr(start))
            val endExpr = generateExpr(end)
            val stepVal =
                if (step eq null)
                    1
                else
                    evalConstExpr(step)
            val cond = gen.Binary(
                if (stepVal > 0) "<=" else ">=",
                gen.Id(varName, id.isByRef),
                endExpr)
            val post = gen.Inc(varName, gen.NumberLit(stepVal))
            gen.For(pre, cond, post, generateStatements(body))
        // Case statements were translated to if statements in the Simplify
        // step
        case _ =>
            println("Unknown: " + stmt)
            null
    }

    def generateExpr(expr: Expression): gen.Expr = expr match {
        case id @ Id(_) if (id.ref ne null) && (id.ref.constVal != None) =>
            gen.NumberLit(id.ref.constVal.get)
        case id @ Id(_) if id.ref eq EnvA1.TRUE =>
            gen.NumberLit(1)
        case id @ Id(_) if id.ref eq EnvA1.FALSE =>
            gen.NumberLit(0)
        case id @ Id(name) =>
            gen.Id(name, id.isByRef)
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
