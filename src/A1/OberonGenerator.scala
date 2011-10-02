package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{GeneratorBase, MainBase, PrettyPrint}

object OberonMain extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new ast.OberonGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            val nameErrors = NameBinding.process(grammar.tree)
            checkErrors(nameErrors)

            println(PrettyPrintOberon.toString(grammar.tree))
        }
    }
}
