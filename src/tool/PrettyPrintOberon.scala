package ee.cyber.simplicitas.oberonexample

import java.io.Writer

import ee.cyber.simplicitas.prettyprint.Doc
import ast._
import Doc._
import BinaryOp.precedence

object PrettyPrintOberon {
    def prettyPrint(module: Module, writer: Writer) {
        val doc = prettyPrint(module)
        println("doc:\n\n" + doc)
        show(doc, 0.8, 75, writer)
    }

    def toString(module: Module) = {
        val doc = prettyPrint(module)
        doc.toString
    }

    implicit def idToDoc(id: Id) = text(id.text)
    def concat(lst: List[Doc]): Doc =
        lst.foldLeft[Doc](empty)(_ :: _)

    def indent(doc: Doc) = Doc.indent(4, doc)

//    implicit def strToDoc(str: String) = text(str)

    def prettyPrint(module: Module): Doc = {
        "MODULE" :+: module.name1 :: semi :#:
            prettyPrint(module.decl) ::
        "BEGIN" :#:
            prettyPrint(module.statements) ::
        "END" :+: module.name2 :: text(".")
    }

    private def prettyPrint(statements: StatementSequence): Doc =
        if (statements ne null)
           indent(
               punctuate(semi :: line,
                   statements.stmt.map(prettyPrint))) ::
           line
        else
            empty

//    private def withSemicolons(lst: List[Doc],
//                               withLineBreak: Boolean = true): Document =
//        withSeparator(lst,
//            semicolon :: (if (withLineBreak) lineBreak else space))

    private def doElsif(elsif: (Expression, StatementSequence)): Doc =
        "ELSIF" :+: prettyPrint(elsif._1) :#:
            prettyPrint(elsif._2)

    private def doCaseClause(clause: CaseClause): Doc = {
        def doConst(c: CaseConstant): Doc =
            c.begin.text ::
                    (if(c.end ne null)
                        ".." :: text(c.end.text)
                    else
                        empty)

        withCommas(clause.items.map(doConst)) :: ":" :#:
            prettyPrint(clause.stmt)
    }

    private def prettyPrint(stmt: Statement): Doc = stmt match {
        case Assignment(left, right) =>
            prettyPrint(left) :+: ":=" :+: prettyPrint(right)
        case ProcedureCall(name, args) =>
            name ::
            (if (args.isEmpty)
                empty
            else
                parens(withCommas(args.map(prettyPrint))))
        case IfStatement(cond, ifStmt, elseStmt) =>
            "IF" :+: prettyPrint(cond.head) :+:
                    "THEN" :#:
                prettyPrint(ifStmt.head) ::
            concat(cond.tail.zip(ifStmt.tail).map(doElsif)) ::
            (if (elseStmt ne null)
                "ELSE" :#:
                        prettyPrint(elseStmt)
            else
                empty) ::
            "END"
        case WhileStatement(cond, body) =>
            "WHILE" :+: prettyPrint(cond) :+:
                    "DO" :#:
                prettyPrint(body) ::
            "END"
        case ForStatement(variable, start, direction, end, body) =>
            "FOR" :+: variable :+: ":=" :+:
                    prettyPrint(start) :+:
                    (direction match {
                        case To() => "TO"
                        case DownTo() => "DOWNTO"
                    }) :+:
                    prettyPrint(end) :+: "DO" :#:
                prettyPrint(body) ::
            "END"
        case CaseStatement(expr, clauses, elseClause) =>
            "CASE" :+: prettyPrint(expr) :+: "OF" :#:
            concat(clauses.map(doCaseClause)) ::
            (if (elseClause ne null)
                "ELSE" :#: prettyPrint(elseClause)
            else
                empty) ::
            "END"
    }

    private def prettyPrint(decl: Declarations): Doc = {
        def doConst(c: ConstantDef) =
            c.name :+: "=" :+: prettyPrint(c.expr)
        def doType(t: TypeDef): Doc =
            t.name :+: "=" :+: t.tValue
        def doVar(v: VarDef): Doc =
            withCommas(v.vars.ids.map(idToDoc)) :: ":" :+: v.varType

        val body =
            (if (decl.consts.isEmpty)
                empty
            else
                "CONST" :#:
                        indent(
                            punctuate(semi :: line,
                                decl.consts.map(doConst)))) :#:
            (if (decl.types.isEmpty)
                empty
            else
                "TYPE" :#:
                    indent(
                        punctuate(semi :: line, decl.types.map(doType)))) :#:
            (if (decl.vars.isEmpty)
                empty
            else
                "VAR" :: line ::
                    indent(
                        punctuate(semi :: line, decl.vars.map(doVar)))) :#:
            punctuate(semi :: line, decl.procedures.map(prettyPrint))

        indent(body) :: line
    }

    private def prettyPrint(proc: ProcedureDecl): Doc = {
        def print(fp: FormalParam): Doc = {
            (if (fp.pVar ne null) text("VAR") else empty) ::
                    withCommas(fp.ids.ids.map(idToDoc)) ::
                    ":" :+: fp.pType
        }
        def params: Doc =
            if ((proc.params ne null) && !proc.params.isEmpty)
                parens(punctuate(semi :: space, proc.params.map(print)))
            else
                empty

        "PROCEDURE" :+: proc.name :: params :: semi :: line ::
        prettyPrint(proc.decl) ::
        (if (proc.body ne null)
            "BEGIN" :#:
                    prettyPrint(proc.body)
        else
            empty) ::
        "END" :+: proc.name2

    }

    private def prettyPrint(expr: Expression): Doc = {
        def wrapIfNeeded(expr: Expression, parentOp: Any) = expr match {
            case Unary(op, _) if (precedence(op) < precedence(parentOp)) =>
                parens(prettyPrint(expr))
            case Binary(op, _, _) if (precedence(op) < precedence(parentOp)) =>
                parens(prettyPrint(expr))
            case _ =>
                prettyPrint(expr)
        }

        expr match {
            case Id(id) => text(id)
            case NumberLit(n) => text(n)
            case Unary(op, arg) =>
                val argPP = arg match {
                    case Binary(_, _, _) =>
                        parens(prettyPrint(arg))
                    case _ =>
                         prettyPrint(arg)
                }
                op.toString :: argPP
            case Binary(op, left, right) =>
                wrapIfNeeded(left, op) :+: op.toString :+:
                        wrapIfNeeded(right, op)
            case _ => text("expr")
        }
    }
}