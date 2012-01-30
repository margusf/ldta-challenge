package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{SourceMessage, MainBase}
import ast.Module
import java.io.{FileWriter, File, Writer}

object OberonMainA4 extends MainBase {
    val grammar = new ast.OberonGrammar()

    def outputFile[Tree](tree: Tree, baseFile: String, suffix: String,
            transform: (Tree, Writer) => Unit) {
        val fileName = baseFile.replaceAll("\\.ob$", suffix)
        val writer = new FileWriter(fileName, false)
        try {
            transform(tree, writer)
        } finally {
            writer.close()
        }
    }

    def generate(fileName: String, tree: Module) {
        Lift.lift(tree)
        outputFile(tree, fileName, "_lifted.ob",
                PrettyPrintOberon.prettyPrint)

        // Simplify the case statements
        Simplify.simplify(tree)
        val cTree = Codegen.generate(tree)
        outputFile(cTree, fileName, ".c",
                PrettyPrintC.prettyPrint)

    }

    def processFile(file: String) {
        Errors.parse(grammar, file)
        OtherChecks.process(grammar.tree)
        NameBindingA4.process(grammar.tree)
        TypecheckA4.process(grammar.tree)
        generate(file, grammar.tree)
    }

    def main(argv: Array[String]) {
        parseOptions(argv)
        for (arg <- sources) {
            Errors.handle(() => processFile(arg))
        }
    }
}
