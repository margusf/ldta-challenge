package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.prettyprint.Document
import Document._
import BinaryOp.precedence
import java.io.Writer

object PrettyPrintOberon {
    def prettyPrint(module: Module, writer: Writer) {
        val doc = prettyPrint(module)
        doc.format(75, writer)
    }

    def toString(module: Module) = {
        val writer = new java.io.StringWriter
        val doc = prettyPrint(module)
        doc.format(40, writer)
        writer.toString
    }

    implicit def idToDoc(id: Id) = text(id.text)
    def concat(lst: List[Document]): Document =
        lst.foldLeft[Document](empty)(_ :: _)
    def nest(doc: Document): Document = Document.nest(4, doc)

//    implicit def strToDoc(str: String) = text(str)

    val semicolon = ";"

    def prettyPrint(module: Module): Document = {
        "MODULE" :: space :: module.name1 :: semicolon :: lineBreak ::
            nest(prettyPrint(module.decl)) :: lineBreak ::
        "BEGIN" :: lineBreak ::
            nest(prettyPrint(module.statements)) ::
        "END" :: space :: module.name2 :: text(".")
    }

    private def prettyPrint(statements: StatementSequence): Document =
        if (statements ne null)
           withSemicolons(statements.stmt.map(prettyPrint)) :: lineBreak
        else
            empty

    private def withSeparator(lst: List[Document], sep: Document): Document = {
        def loop(lst: List[Document]): Document = lst match {
            case Nil =>
                empty
            case h :: Nil =>
                h
            case h :: t =>
                h :: sep :: loop(t)
        }

        loop(lst)
    }

    private def withCommas(lst: List[Document]): Document =
        withSeparator(lst, "," :: space)

    private def withSemicolons(lst: List[Document],
                               withLineBreak: Boolean = true): Document =
        withSeparator(lst,
            semicolon :: (if (withLineBreak) lineBreak else space))

    private def doElsif(elsif: (Expression, StatementSequence)): Document =
        "ELSIF" :: space :: prettyPrint(elsif._1) :: lineBreak ::
            nest(prettyPrint(elsif._2)) :: lineBreak

    private def doCaseClause(clause: CaseClause): Document = {
        def doConst(c: CaseConstant): Document =
            c.begin.text ::
                    (if(c.end ne null)
                        ".." :: text(c.end.text)
                    else
                        empty)

        withCommas(clause.items.map(doConst)) :: ":" :: lineBreak ::
            nest(prettyPrint(clause.stmt))
    }

    private def prettyPrint(stmt: Statement): Document = stmt match {
        case Assignment(left, right) =>
            prettyPrint(left) :: space :: ":=" :: space :: prettyPrint(right)
        case ProcedureCall(name, args) =>
            name ::
            (if (args.isEmpty)
                empty
            else
                "(" :: withCommas(args.map(prettyPrint)) :: text(")"))
        case IfStatement(cond, ifStmt, elseStmt) =>
            "IF" :: space :: prettyPrint(cond.head) :: space ::
                    "THEN" :: lineBreak ::
                nest(prettyPrint(ifStmt.head)) ::
            concat(cond.tail.zip(ifStmt.tail).map(doElsif)) ::
            (if (elseStmt ne null)
                "ELSE" :: lineBreak ::
                        nest(prettyPrint(elseStmt))
            else
                empty) ::
            text("END")
        case WhileStatement(cond, body) =>
            "WHILE" :: space :: prettyPrint(cond) :: space ::
                    "DO" :: lineBreak ::
                nest(prettyPrint(body)) ::
            text("END")
        case ForStatement(variable, start, direction, end, body) =>
            "FOR" :: space :: variable :: space :: ":=" ::
                    space :: prettyPrint(start) :: space ::
                    (direction match {
                        case To() => "TO"
                        case DownTo() => "DOWNTO"
                    }) :: space ::
                    prettyPrint(end) :: space :: "DO" :: lineBreak ::
                nest(prettyPrint(body)) ::
            text("END")
        case CaseStatement(expr, clauses, elseClause) =>
            "CASE" :: space :: prettyPrint(expr) ::
                    space :: "OF" :: lineBreak ::
            concat(clauses.map(doCaseClause)) ::
            (if (elseClause ne null)
                "ELSE" :: lineBreak :: nest(prettyPrint(elseClause))
            else
                empty) ::
            text("END")
        case _ => text("stmt")
    }

    private def prettyPrint(decl: Declarations): Document = {
        def doConst(c: ConstantDef) =
            c.name :: space :: "=" :: space ::
                    prettyPrint(c.expr) :: semicolon :: lineBreak
        def doType(t: TypeDef): Document =
            t.name :: space :: "=" :: space :: t.tValue ::
                    semicolon :: lineBreak
        def doVar(v: VarDef): Document =
            withCommas(v.vars.ids.map(idToDoc)) ::
            ":" :: space :: v.varType :: semicolon :: lineBreak

        (if (decl.consts.isEmpty)
            empty
        else
            "CONST" :: lineBreak ::
                    nest(concat(decl.consts.map(doConst)))) ::
        (if (decl.types.isEmpty)
            empty
        else
            "TYPE" :: lineBreak ::
                nest(concat(decl.types.map(doType)))) ::
        (if (decl.vars.isEmpty)
            empty
        else
            "VAR" :: lineBreak ::
                nest(concat(decl.vars.map(doVar)))) ::
        withSemicolons(decl.procedures.map(prettyPrint))
    }

    private def prettyPrint(proc: ProcedureDecl): Document = {
        def print(fp: FormalParam): Document = {
            (if (fp.pVar ne null) text("VAR") else empty) ::
                    withCommas(fp.ids.ids.map(idToDoc)) ::
                    ":" :: space :: fp.pType
        }
        def params: Document =
            if ((proc.params ne null) && !proc.params.isEmpty)
                "(" :: withSemicolons(proc.params.map(print), false) ::
                        text(")")
            else
                empty

        "PROCEDURE" :: space :: proc.name :: params :: semicolon :: lineBreak ::
        nest(prettyPrint(proc.decl)) ::
        (if (proc.body ne null)
            "BEGIN" :: lineBreak ::
                    nest(prettyPrint(proc.body))
        else
            empty) ::
        "END" :: space :: proc.name2

    }

    private def prettyPrint(expr: Expression): Document = {
        def wrapIfNeeded(expr: Expression, parentOp: Any) = expr match {
            case Unary(op, _) if (precedence(op) < precedence(parentOp)) =>
                "(" :: prettyPrint(expr) :: text(")")
            case Binary(op, _, _) if (precedence(op) < precedence(parentOp)) =>
                "(" :: prettyPrint(expr) :: text(")")
            case _ =>
                prettyPrint(expr)
        }

        expr match {
            case Id(id) => text(id)
            case NumberLit(n) => text(n)
            case Unary(op, arg) =>
                val argPP = arg match {
                    case Binary(_, _, _) =>
                        "(" :: prettyPrint(arg) :: text(")")
                    case _ =>
                         prettyPrint(arg)
                }
                op.toString :: argPP
            case Binary(op, left, right) =>
                wrapIfNeeded(left, op) :: space :: op.toString ::
                        space :: wrapIfNeeded(right, op)
            case _ => text("expr")
        }
    }
}