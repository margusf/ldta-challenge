package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.MainBase

object OberonMainA1 extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new ast.OberonGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            if (!grammar.errors.isEmpty) {
                println("parse failed")
            } else {
                val otherErrors = OtherChecks.process(grammar.tree)
                if (!otherErrors.isEmpty) {
                    println("parse failed")
                } else {
                    val nameErrors = NameBindingA1.process(grammar.tree)
                    checkErrors(nameErrors)
                }
            }
        }
    }
}
