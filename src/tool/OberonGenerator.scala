package ee.cyber.simplicitas.oberonexample;

import ee.cyber.simplicitas.{GeneratorBase, MainBase, PrettyPrint}

class OberonGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  def generate(tree: Module) {
    println(PrettyPrint.prettyPrint(tree))
  }
}
  
object OberonMain extends MainBase {
  def main(argv: Array[String]) {
    parseOptions(argv)
    val grammar = new OberonGrammar()
    for (arg <- sources) {
      grammar.parseFile(arg)
      checkErrors(grammar.errors)
      
      new OberonGenerator(destDir).generate(grammar.tree)        
    }
  }
}
