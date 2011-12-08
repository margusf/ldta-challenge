package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, LiteralNode}

// Flattens the nested procedures.
object Lift {
    def lift(module: Module) {
        val lift = new Lift(module)
        lift()
    }
}

class Lift(module: Module) {
    // List of top-level procedures (will be filled when procedure
    // declarations are flattened)
    val topLevel = ArrayBuffer[ProcedureDecl]()
    val topTypes = ArrayBuffer[TypeDef]()
    val topIds = collection.mutable.Set[String]()

    // TODO: also lift types!
    def apply() {
        module.decl.types.foreach(doType(null))
        module.decl.procedures.foreach(doProcedure(null))

        module.decl.procedures = topLevel.toList
        module.decl.types = topTypes.toList

        module.walkTree(fixReferences)
    }

    private def doProcedure(prefix: String)(proc: ProcedureDecl) {
        val myNewName = newLiftedName(prefix, proc.name.text)

        proc.decl.types.foreach(doType(myNewName))
        proc.decl.procedures.foreach(doProcedure(myNewName))

        // All the child procedures and types are removed.
        proc.decl.procedures = Nil
        proc.decl.types = Nil

        // Add main procedure after children. This preserves the scopes.
        proc.name.text = myNewName
        proc.name2.text = myNewName
        addToTop(proc)

    }

    private def doType(prefix: String)(td: TypeDef) {
        val newTypeName = newLiftedName(prefix, td.name.text)
        td.name.text = newTypeName
        addToTop(td)
    }

    private def addToTop(proc: ProcedureDecl) {
        topLevel += proc
        topIds += proc.name.text
    }

    private def addToTop(td: TypeDef) {
        topTypes += td
        topIds += td.name.text
    }

    private def newLiftedName(prefix: String, name: String) = {
        def makeUnique(s: String) = {
            if (!topIds(s))
                s
            else {
                var n = 1
                while (topIds(s + n))
                    n += 1
                s + n
            }
        }

        if (prefix eq null)
            makeUnique(name)
        else
            makeUnique(prefix + name.capitalize)
    }

    private def fixReferences(node: CommonNode) {
        node match {
            case id @ Id(_) =>
                val original = id.ref
                if ((original ne null) && (original.text != id.text)) {
                    id.text = original.text
                }
            case _ =>
                ()
        }
    }
}
