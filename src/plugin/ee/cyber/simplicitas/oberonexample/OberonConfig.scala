package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.{GenericToken, CommonNode}

import org.eclipse.swt.graphics.Image
import org.eclipse.imp.language.LanguageRegistry


object OberonConfig {
    // These four values serve technical purposes, do not touch them.
    val instance = new OberonConfig()
    val languageId = "oberon0_example"
    val language = LanguageRegistry.findLanguage(languageId)
    val pluginId = "oberon0_example"

    /** Insert here default values by color symbols that will be
     * returned by the getTokenColor method. Map key is the color
     * symbol and value is description of the color symbol, consisting of:
     * - human-readable name of the color that is shown in the preferences
     *   dialog.
     * - default value of the color, encoded as "R,G,B"
     *   (e.g., "255, 255, 0" for yellow).
     * - font style using SWT constants (e.g, SWT.BOLD | SWT.ITALIC)
     */
    val colors: Map[Symbol, Tuple3[String, String, Number]] =
        Map.empty

    /** This object contains references to images that are loaded via
     * plugin's ImageRegistry. The image objects are declared in this object
     * and initialized in the <code>initializeImages</code> method. */
    object Images {
        /* var imageX: Image = null */
    }

    /** Load images that will be used as icons in the <code>treeImage</code>
     * and <code>fileImage</code> methods.
     * This method is passed two-argument function <code>addFun</code>
     * that is used to actually load images. It takes two arguments.
     * The first argument is key that is used in the plugin's ImageRegistry.
     * (you do not need to do anything with they key except ensure that every
     * image has unique key). The second argument is relative path to the
     * image file. The path is relative to root of the plugin's jar.
     * The function returns Image object that can later be used in
     * <code>treeImage</code> and <code>fileImage</code> methods.
     */
    def initializeImages(addFun: (String, String) => Image) {
        /* Images.imageX = addFun("imageX", "icons/imageX.gif") */
    }
}

class OberonConfig extends APluginConfig {
    /** Parses the input using the correct grammar. */
    def parse(ctx: ParseCtx) {
        val grammar = new OberonGrammar()
        ctx.parse(grammar)
        val typeErrors = Typecheck.process(grammar.tree)
        ctx.reportErrors(typeErrors)
    }

    /** There is nothing to show in the outline view. */
    def treeLabel(node: CommonNode) = node match {
        case Module(Id(name), _, _, _) => "MODULE " + name
        case ProcedureDecl(Id(name), _, _, _, _, _) => "PROCEDURE " + name
        case ConstantDef(Id(name), _, _) => "CONST " + name
        case Id(name) if node.parent.isInstanceOf[IdentList]  &&
                node.parent.parent.isInstanceOf[VarDef] => "VAR " + name
        case _ => null
    }


    override def referenceTarget(node: CommonNode) = node match {
        case id: Id => id.ref
        case _ => null
    }

    override def runGenerator(dir: String, file: String) {
        OberonMain.main(Array("--dest", dir, dir + file))
    }
}
