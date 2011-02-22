// This file contains generated code. You are not supposed to edit it.
// Instead, use the file OberonConfig.scala to customize your DSL IDE.

package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.imp._
import ee.cyber.simplicitas.imp.parser._
import ee.cyber.simplicitas.eclipse.GenerateAction

import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT


object OberonPlugin {
  var instance: OberonPlugin = null
  
  def getInstance =
    if (instance eq null)
      new OberonPlugin()
    else
      instance
  
  val factory = () => getInstance
}

class OberonPlugin extends SimplicitasPlugin {
  def getID = OberonConfig.pluginId
  def getLanguageID = OberonConfig.languageId

  OberonPlugin.instance = this
  
  colorCache = collection.mutable.Map()

  colorDefs =
    Map[Symbol, Tuple3[String, String, Number]](
      'keyword -> ("Keywords", "128,0,128", SWT.BOLD),
      'operator -> ("Operators", "0,0,0", SWT.NORMAL),
      'comment -> ("Comments", "128,128,0", SWT.ITALIC)) ++
    OberonConfig.colors

  override def initializeImageRegistry(registry: ImageRegistry) {
    super.initializeImageRegistry(registry)

    val bundle = org.eclipse.core.runtime.Platform.getBundle(
            OberonConfig.pluginId);
    val addFun =
      (key: String, path: String) =>
        addImage(key, path, bundle, registry)
    OberonConfig.initializeImages(addFun)
  }
}

class OberonParseController extends 
  SimplicitasParseController(OberonConfig.language, OberonConfig.instance) {}

class OberonTokenColorer extends TokenColorerBase(OberonPlugin.factory,
                                                    OberonConfig.instance) {}

class OberonTreeModelBuilder
  extends SimplicitasTreeModelBuilder(OberonConfig.instance) {
}

class OberonLabelProvider
    extends SimplicitasLabelProvider(OberonConfig.instance) {}

class OberonReferenceResolver
    extends SimplicitasReferenceResolver(OberonConfig.instance) {}

class OberonFoldingUpdater
    extends SimplicitasFoldingUpdater(OberonConfig.instance) {}

class OberonDocumentationProvider
    extends SimplicitasDocumentationProvider(OberonConfig.instance) {}

class OberonContentProposer
    extends SimplicitasContentProposer(OberonConfig.instance) {}

class OberonOccurrenceMarker
    extends SimplicitasOccurrenceMarker(OberonConfig.instance) {}

class OberonPreferencePage
    extends SimplicitasPreferencePage(OberonPlugin.factory) {}

class OberonPreferenceInitializer
    extends SimplicitasPreferenceInitializer(OberonPlugin.factory) {}

class OberonGenerateAction extends GenerateAction(OberonConfig.instance) {}