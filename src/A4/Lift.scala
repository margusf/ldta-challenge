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
    val topIds = collection.mutable.Set[String]()

    def apply() {
        module.decl.procedures.foreach(doProcedure(null))

        module.walkTree(fixProcedureCall)

        module.decl.procedures = topLevel.toList
    }

    private def doProcedure(prefix: String)(proc: ProcedureDecl) {
        val myNewName = newLiftedName(prefix, proc.name.text)
        proc.name.text = myNewName
        addToTop(proc)

        proc.decl.procedures.foreach(doProcedure(myNewName))
        // All the child procedures are removed.
        proc.decl.procedures = Nil
    }

    private def addToTop(proc: ProcedureDecl) {
        topLevel += proc
        topIds += proc.name.text
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

    private def fixProcedureCall(node: CommonNode) {
        node match {
            case call @ ProcedureCall(name, _) =>
                val original = name.ref.asInstanceOf[Id]
                name.text = original.text
            case _ =>
                ()
        }
    }
}