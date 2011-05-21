package ee.cyber.simplicitas.oberonexample.gen

import collection.mutable.ArrayBuffer

abstract class Gen {
    def gen(buf: StringBuilder) {}
}

case class Module(name: String,
                  consts: List[ConstDecl],
                  vars: List[VarDecl],
                  procedures: List[ProcDecl])

case class ConstDecl(name: String, cType: String, value: Expr) extends Statement
case class VarDecl(name: String, vType: String) extends Statement
abstract class Expr
case class ProcDecl(name: String,
                    args: List[String],
                    argTypes: List[String],
                    body: List[Statement])
abstract class Statement extends Gen
