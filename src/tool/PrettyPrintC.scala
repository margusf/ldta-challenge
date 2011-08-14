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
        hcat(module.consts.map(prettyPrint)) :#:
        hcat(module.vars.map(prettyPrint)) :#:
        hcat(module.procedures.map(prettyPrint))
    }

    private def indent(doc: Doc) = Doc.indent(4, doc)

    private def prettyPrint(cd: ConstDecl): Doc =
        text("const")

    private def prettyPrint(cd: VarDecl): Doc =
        text("var")

    private def prettyPrint(cd: ProcDecl): Doc = {
        val args = cd.args.map(prettyPrint)

        "void" :+: cd.name :: parens(withCommas(args)) :+: "{" :#:
            indent(vcat(cd.body.map(prettyPrint))) :#:
        text("}")
    }

    private def prettyPrint(arg: Arg): Doc =
        arg.argType :+: text(arg.name)

    private def prettyPrint(stmt: Stmt): Doc = stmt match {
        case Nop() => empty
        case Sequence(items) =>
            vcat(items.map(prettyPrint))
        case Assign(id, value) =>
            id :+: "=" :+: prettyPrint(value) :: semi
        case If(cond, ifStmt, elseStmt) =>
            "if" :+: parens(prettyPrint(cond)) :+: "{" :#:
                indent(prettyPrint(ifStmt)) :#:
            (if (elseStmt ne null)
                "}" :+: "else" :+: "{" :#:
                    indent(prettyPrint(ifStmt)) :: line
            else
                empty) ::
            text("}")
        case ConstDecl(name, cType, value) =>
            cType :+: name :+: "=" :+: prettyPrint(value) :: semi
        case VarDecl(name, vType) =>
            vType :+: name :: semi
        case expr: Expr =>
            prettyPrint(expr) :: semi
        case _ => text("stmt") :: semi
    }

    private def prettyPrint(expr: Expr): Doc = {
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
            case Id(name) => text(name)
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