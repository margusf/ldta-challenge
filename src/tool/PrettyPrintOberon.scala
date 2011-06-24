package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.prettyprint.Document
import Document._
import java.io.Writer

object PrettyPrintOberon {
    val indent = 4

    def prettyPrint(module: Module, writer: Writer) {
        val doc = prettyPrint(module)
        doc.format(75, writer)
    }

    def toString(module: Module) = {
        val writer = new java.io.StringWriter
        val doc = prettyPrint(module)
        doc.format(75, writer)
        writer.toString
    }

    implicit def idToDoc(id: Id) = text(id.text)

    val semicolon = ";" :: lineBreak

    def prettyPrint(module: Module): Document = {
        "MODULE" :: space :: module.name1 :: semicolon ::
            // declarations
        "BEGIN" :: lineBreak ::
            // statements
        "END" :: space :: module.name2 :: text(".")
    }
}