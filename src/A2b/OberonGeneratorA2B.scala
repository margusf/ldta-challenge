package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{SourceMessage, MainBase}

object OberonMainA2B extends MainBase {
    def logErrors(fileName: String, errors: Seq[SourceMessage]) {
        if (!errors.isEmpty) {
            var pw = new java.io.PrintWriter(
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
                    NameBindingA1.process(grammar.tree) match {
                        case Some(msg) =>
                            println("line: " + msg.startLine +
                                    "  " + msg.message)
                        case None =>
                            TypecheckA2B.process(grammar.tree) match {
                                case Some(msg) =>
                                    println("line: " + msg.startLine +
                                            "  " + msg.message)
                                case None =>
                                    ()
                            }
                    }
                }
            }
        }
    }
}
