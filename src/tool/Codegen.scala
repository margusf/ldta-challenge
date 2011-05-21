package ee.cyber.simplicitas.oberonexample

import gen.Gen

object Codegen {
    def generate(module: Module) {
        val code = generateModule(module)

        println(code)
    }

    private def generateModule(module: Module): Gen = {
        println(generateDecl(module.decl))

        val stmt = generateStatement(module.statements)
        println(stmt)

        gen.Module(module.name1.text, null, null, null)
        null
    }

    private def generateDecl(decl: Declarations): Gen = {
        null
    }

    private def generateStatement(stmt: StatementSequence): Gen = {
        null
    }
}