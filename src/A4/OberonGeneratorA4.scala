package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{SourceMessage, MainBase}
import ast.Module
import java.io.{FileWriter, File, Writer}

object OberonMainA4 extends MainBase {
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

    def logErrors(fileName: String, errors: Seq[SourceMessage]) {
        if (!errors.isEmpty) {
            val pw = new java.io.PrintWriter(
                    new java.io.FileWriter("/tmp/oberon-log.txt", true), true)
            pw.println("\nFile: " + fileName)
            errors foreach (pw.println)
            pw.close()
        }
    }

    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new ast.OberonGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            if (!grammar.errors.isEmpty) {
                println("parse failed")
                logErrors(arg, grammar.errors)
            } else {
                val otherErrors = OtherChecks.process(grammar.tree)
                if (!otherErrors.isEmpty) {
                    println("parse failed")
                    logErrors(arg, otherErrors)
                } else {
                    NameBindingA4.process(grammar.tree) match {
                        case Some(msg) =>
                            println("line: " + msg.startLine +
                                    "  " + msg.message)
                        case None =>
                            TypecheckA4.process(grammar.tree) match {
                                case Some(msg) =>
                                    println("line: " + msg.startLine +
                                            "  " + msg.message)
                                case None =>
                                    generate(arg, grammar.tree)
                            }
                    }
                }
            }
        }
    }
}
