package ee.cyber.simplicitas.oberonexample;

import ee.cyber.simplicitas.{GeneratorBase, MainBase}

class OberonGenerator(destDir: String) 
        extends GeneratorBase(destDir) {
  val templates = getTemplates("Oberon.stg")
    
  def generate(tree: Program) {
    val args = tree.toJavaMap()
    writeFile("GeneratedProgram.java", templates.getInstanceOf("program", args))
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
