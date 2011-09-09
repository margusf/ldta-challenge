package ee.cyber.simplicitas.oberonexample

import java.io.Writer
import ee.cyber.simplicitas.prettyprint.Doc
import Doc._
import gen._


object PrettyPrintC {
    def prettyPrint(module: gen.Module, writer: Writer) {
        val doc = prettyPrint(module)
        show(doc, 0.8, 75, writer)
    }

    def toString(module: Module) = {
        val doc = prettyPrint(module)
        doc.toString
    }

    def prettyPrint(module: Module): Doc = {
        "// Module " :: module.name :#:
        "#include <stdio.h>" :#:
        vcat(module.globals.map(prettyPrint)) :#:
        vcat(module.procedures.map(prettyPrint))
    }

    private def indent(doc: Doc) = Doc.indent(4, doc)

    private def prettyPrint(cd: ProcDecl): Doc = {
        val args = cd.args.map(
            (arg: Arg) => prettyPrint(arg.name, arg.argType))

        "void" :+: cd.name :: parens(withCommas(args)) :+: "{" :#:
            indent(vcat(cd.body.map(prettyPrint))) :#:
        text("}")
    }

    private def prettyPrint(stmt: Stmt): Doc = stmt match {
        case Nop() => empty
        case Sequence(items) =>
            vcat(items.map(prettyPrint))
        case Assign(lhs, value) =>
            prettyPrint(lhs) :+: "=" :+: prettyPrint(value) :: semi
        case If(cond, ifStmt, elseStmt) =>
            "if" :+: parens(prettyPrint(cond)) :+: "{" :#:
                indent(prettyPrint(ifStmt)) :#:
            "}" :+:
            (elseStmt match {
                case null =>
                    empty
                case If(_, _, _) =>
                    "else" :+: prettyPrint(elseStmt)
                case _ =>
                    "else" :+: "{" :#:
                        indent(prettyPrint(elseStmt)) :#:
                    text("}")
            })
        case While(cond, body) =>
            "while" :+: parens(prettyPrint(cond)) :+: "{" :#:
                indent(prettyPrint(body)) :#:
            text("}")
        case For(pre, cond, post, body) =>
            "for" :+: parens(
                    prettyPrint(pre) :+:
                    prettyPrint(cond) :: semi :+:
                    prettyPrintFp(post)) :+: "{" :#:
                indent(prettyPrint(body)) :#:
            text("}")
        case ConstDecl(name, cType, value) =>
            prettyPrint(name, cType) :+: "=" :+: prettyPrint(value) :: semi
        case VarDecl(name, vType) =>
            prettyPrint(name, vType) :: semi
        case Typedef(name, tType) =>
            "typedef" :+: prettyPrint(text(name), tType) :: semi
        case expr: Expr =>
            prettyPrint(expr) :: semi
        case fp: ForPost =>
            prettyPrintFp(fp) :: semi
        case _ =>
            println("Unknown: " + stmt)
            text("stmt") :: semi
    }

    private def prettyPrint(f: OField): Doc =
        prettyPrint(f.name, f.fType) :: semi

    private def prettyPrint(arg: Doc, t: OType): Doc = t match {
        case ORef(id) => id :+: arg
        case OCArray(base @ OCArray(_, _), size) =>
            prettyPrint(arg :: brackets(prettyPrint(size)), base)
        case OCArray(base, size) =>
            prettyPrint(arg, base) :: brackets(prettyPrint(size))
        case ORecord(fields) =>
            "struct" :+: "{" :#:
                indent(vcat(fields.map(prettyPrint).toList)) :#:
            "}" :+: arg
        case _ => text(t.toString) :+: arg
    }

    private def prettyPrintFp(fp: ForPost): Doc = fp match {
        case Inc(id, value) =>
            id :+: "+=" :+: prettyPrint(value)
        case Dec(id, value) =>
            id :+: "-=" :+: prettyPrint(value)
    }

    private def prettyPrint(expr: Expr): Doc = {
        // TODO: copypaste of Oberon pretty-printer
        def wrapIfNeeded(expr: Expr, parentOp: String) = expr match {
            case Unary(op, _) if (precedence(op) < precedence(parentOp)) =>
                parens(prettyPrint(expr))
            case Binary(op, _, _) if (precedence(op) < precedence(parentOp)) =>
                parens(prettyPrint(expr))
            case _ =>
                prettyPrint(expr)
        }

        expr match {
            case FunCall(name, args) =>
                name :: parens(withCommas(args.map(prettyPrint)))
            case Id(name, isRef) =>
                if (isRef)
                    parens("*" :: text(name))
                else
                    text(name)
            case NumberLit(value) => text(value.toString)
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
            case RecordAccess(rec, field) =>
                prettyPrint(rec) :: "." :: text(field)
            case ArrayAccess(array, index) =>
                prettyPrint(array) :: brackets(prettyPrint(index))
            case _ => text("expr")
        }
    }

    val precedence = Map(
        "||" -> 1,
        "&&" -> 2,
        "<" -> 3, "<=" -> 3, ">" -> 3, ">=" -> 3, "==" -> 3, "!=" -> 3,
        "" -> 4,
        "+" -> 4, "-" -> 4,
        "/" -> 5, "*" -> 5, "%" -> 5,
        "!" -> 6)
}