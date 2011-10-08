package ee.cyber.simplicitas.oberonexample

import java.io.Writer

import ee.cyber.simplicitas.prettyprint.Doc
import ast._
import Doc._
import BinaryOp.precedence

object PrettyPrintOberon {
    // TODO, XXX: these should be punctuate and withCommas from the
    // official PPrint class, but for some reason these will
    // cause infinite loop.
    def punctuate(sep: Doc , items: List[Doc]): Doc = {
        def loop(lst: List[Doc]): Doc = lst match {
            case Nil => empty
            case List(d) => d
            case h :: t => h :: sep :: loop(t)
        }

        loop(items)
    }

    def withCommas(lst: List[Doc]): Doc =
        punctuate(comma :: space, lst)

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

    private def doElsif(elsif: (Expression, StatementSequence)): Doc =
        "ELSIF" :+: prettyPrint(elsif._1) :#:
            prettyPrint(elsif._2)

    private def prettyPrint(stmt: Statement): Doc = stmt match {
        case Assignment(left, right) =>
            prettyPrint(left) :+: ":=" :+: prettyPrint(right)
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
    }

    private def prettyPrint(tv: TypeValue): Doc = tv match {
        case Id(name) =>
            text(name)
    }

    private def prettyPrint(decl: Declarations): Doc = {
        def doConst(c: ConstantDef) =
            c.name :+: "=" :+: prettyPrint(c.expr) :: semi
        def doType(t: TypeDef): Doc =
            t.name :+: "=" :+: prettyPrint(t.tValue) :: semi
        def doVar(v: VarDef): Doc =
            withCommas(v.vars.ids.map(idToDoc)) :: ":" :+:
                    prettyPrint(v.varType) :: semi

        val body =
            (if (decl.consts.isEmpty)
                empty
            else
                "CONST" :#:
                        indent(
                            vcat(decl.consts.map(doConst)))) :#:
            (if (decl.types.isEmpty)
                empty
            else
                "TYPE" :#:
                    indent(
                        vcat(decl.types.map(doType)))) :#:
            (if (decl.vars.isEmpty)
                empty
            else
                "VAR" :: line ::
                    indent(
                        vcat(decl.vars.map(doVar))))

        indent(body) :: line
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