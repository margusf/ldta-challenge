package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.{SourceMessage, CommonNode}
import collection.mutable.ArrayBuffer

object OtherChecks {
    def process(module: Module) {
        module.walkTree(doNode)
    }

    def doNode(node: CommonNode) {
        node match {
            // check numerical overflow.
            case nl @ NumberLit(txt) =>
                try {
                    txt.toInt
                } catch {
                    case e: NumberFormatException =>
                        throw new ParseError()
                }
            case _ => ()
        }
    }
}