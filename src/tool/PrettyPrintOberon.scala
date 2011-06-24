package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.prettyprint.Document
import Document._
import java.io.Writer

object PrettyPrintOberon {
    val indent = 4

    def prettyPrint(module: Module, writer: Writer) {
        val doc = prettyPrint(module)
        doc.format(75, writer)
    }

    def toString(module: Module) = {
        val writer = new java.io.StringWriter
        val doc = prettyPrint(module)
        doc.format(75, writer)
        writer.toString
    }

    implicit def idToDoc(id: Id) = text(id.text)
    def concat(lst: List[Document]): Document =
        lst.foldLeft[Document](empty)(_ :: _)

//    implicit def strToDoc(str: String) = text(str)

    val semicolon = ";"

    def prettyPrint(module: Module): Document = {
        "MODULE" :: space :: module.name1 :: semicolon :: lineBreak ::
            prettyPrint(module.decl) :: lineBreak ::
        "BEGIN" :: lineBreak ::
            prettyPrint(module.statements) ::
        "END" :: space :: module.name2 :: text(".")
    }

    private def prettyPrint(statements: StatementSequence): Document =
        if (statements ne null)
           withSemicolons(statements.stmt.map(prettyPrint))
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

    private def withSemicolons(lst: List[Document],
                               withLineBreak: Boolean = true): Document =
        withSeparator(lst,
            semicolon :: (if (withLineBreak) lineBreak else space))

    private def prettyPrint(stmt: Statement): Document = stmt match {
        case _ => text("stmt")
    }

    private def prettyPrint(decl: Declarations): Document = {
        def doConst(c: ConstantDef) = {
            c.name :: space :: "=" :: space ::
                    prettyPrint(c.expr) :: semicolon :: lineBreak
        }

        (if (decl.consts.isEmpty)
            empty
        else
            "CONST" :: lineBreak ::
                    concat(decl.consts.map(doConst))) ::
        (if (decl.types.isEmpty)
            empty
        else
            "TYPE" :: lineBreak ::
                // types
                empty
                ) ::
        (if (decl.vars.isEmpty)
            empty
        else
            "CONST" :: lineBreak ::
                // vars
                empty
                ) ::
        withSemicolons(decl.procedures.map(prettyPrint))
    }

    private def prettyPrint(proc: ProcedureDecl): Document = {
        def print(fp: FormalParam): Document = {
            (if (fp.pVar ne null) text("VAR") else empty) ::
                    withSeparator(fp.ids.ids.map(idToDoc),
                        text(",") :: space) ::
                    ":" :: space :: fp.pType
        }
        def params: Document =
            if ((proc.params ne null) && !proc.params.isEmpty)
                "(" :: withSemicolons(proc.params.map(print), false) ::
                        text(")")
            else
                empty

        "PROCEDURE" :: space :: proc.name :: params :: semicolon :: lineBreak ::
        prettyPrint(proc.decl) ::
        (if (proc.body ne null)
            "BEGIN" :: lineBreak ::
                    prettyPrint(proc.body)
        else
            empty) ::
        "END" :: space :: proc.name2

    }

    private def prettyPrint(expr: Expression): Document = expr match {
        case _ => text("expr")
    }
}