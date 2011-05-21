package ee.cyber.simplicitas.oberonexample.gen

abstract class Gen {
    def gen(buf: StringBuilder) {}
}

case class Module(name: String,
                  consts: List[ConstDecl],
                  vars: List[VarDecl],
                  procedures: List[ProcDecl])

case class ConstDecl(name: String, cType: String, value: Expr)
case class VarDecl(name: String, vType: String)
abstract class Expr
case class ProcDecl(name: String,
                    args: List[String],
                    argTypes: List[String],
                    body: List[Statement])
abstract class Statement

