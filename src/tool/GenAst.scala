package ee.cyber.simplicitas.oberonexample.gen

import collection.mutable.ArrayBuffer

abstract class Gen {
    def gen(buf: StringBuilder) {}
}

case class Module(name: String,
                  consts: List[ConstDecl],
                  vars: List[VarDecl],
                  procedures: List[ProcDecl])

case class ConstDecl(name: String, cType: String, value: Expr) extends Stmt
case class VarDecl(name: String, vType: String) extends Stmt
case class ProcDecl(name: String,
                    args: List[String],
                    argTypes: List[String],
                    body: List[Stmt])

abstract class Stmt extends Gen
case class Nop() extends Stmt
case class Sequence(items: Seq[Stmt]) extends Stmt
case class Assign(id: String, value: Expr) extends Stmt
case class If(cond: Expr, ifStmt: Stmt, elseStmt: Stmt) extends Stmt

abstract class Expr extends Stmt // all expressions can be used as statements
case class FunCall(name: String, args: Seq[Expr]) extends Expr