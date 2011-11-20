package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.{GeneratorBase, MainBase, PrettyPrint}

class OberonGenerator(destDir: String)
        extends GeneratorBase(destDir) {
    def generate(sourceFile: String, tree: ast.Module) {
        val simplified = Simplify.simplify(tree)
        println(PrettyPrintOberon.toString(simplified))

        val cFile = sourceFile.replaceAll(".ob$", ".c")
        val genTree = Codegen.generate(simplified)
        writeFile(cFile, PrettyPrintC.toString(genTree))
    }
}

object OberonMainA4 extends MainBase {
    def main(argv: Array[String]) {
        parseOptions(argv)
        val grammar = new ast.OberonGrammar()
        for (arg <- sources) {
            grammar.parseFile(arg)
            checkErrors(grammar.errors)

            val typeErrors = Typecheck.process(grammar.tree)
            checkErrors(typeErrors)

            new OberonGenerator(destDir).generate(arg, grammar.tree)
        }
    }
}
