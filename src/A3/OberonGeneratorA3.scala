/*
 * // Copyright (c) 2010 Cybernetica AS / STACC
 */

package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{SourceMessage, MainBase}

object OberonMainA3 extends MainBase {
    val grammar = new ast.OberonGrammar()

    def processFile(file: String) {
        Errors.parse(grammar, file)
        OtherChecks.process(grammar.tree)
        NameBindingA2A.process(grammar.tree)
        TypecheckA3.process(grammar.tree)
    }

    def main(argv: Array[String]) {
        parseOptions(argv)
        for (arg <- sources) {
            Errors.handle(() => processFile(arg))
        }
    }
}
