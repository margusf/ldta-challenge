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
            indent("...") :#:
        text("}")
    }

    private def prettyPrint(arg: Arg): Doc =
        arg.argType :+: text(arg.name)
}