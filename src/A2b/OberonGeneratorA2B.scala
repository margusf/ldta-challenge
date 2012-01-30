package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{SourceMessage, MainBase}

object OberonMainA2B extends MainBase {
    val grammar = new ast.OberonGrammar()

    def processFile(file: String) {
        Errors.parse(grammar, file)
        OtherChecks.process(grammar.tree)
        NameBindingA1.process(grammar.tree)
        TypecheckA2B.process(grammar.tree)
    }

    def main(argv: Array[String]) {
        parseOptions(argv)
        for (arg <- sources) {
            Errors.handle(() => processFile(arg))
        }
    }
}
