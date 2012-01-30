package ee.cyber.simplicitas.oberonexample

import ast.Id
import ee.cyber.simplicitas.SourceLocation

case class NameError(id: Id) extends Exception
case class TypeError(location: SourceLocation, msg: String) extends Exception
case class ParseError() extends Exception

object Errors {
    def parse(grammar: ast.OberonGrammar, file: String) = {
        val tree = grammar.parseFile(file)
        if (!grammar.errors.isEmpty) {
            throw new ParseError
        }
        tree
    }

    def handle(proc: () => Unit) {
        try {
            proc()
        } catch {
            case ParseError() =>
                println("parse failed")
            case NameError(id: Id) =>
                println("line: " + id.startLine +
                        " Invalid identifier: " + id.text)
            case TypeError(location, msg) =>
                println("line: " + location.startLine + "  " + msg)
        }
    }
}