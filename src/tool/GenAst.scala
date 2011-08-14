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
                    args: List[Arg],
                    body: List[Stmt])
case class Arg(name: String, argType: String)

abstract class Stmt extends Gen
case class Nop() extends Stmt
case class Sequence(items: List[Stmt]) extends Stmt
case class Assign(id: String, value: Expr) extends Stmt
case class If(cond: Expr, ifStmt: Stmt, elseStmt: Stmt) extends Stmt

abstract class Expr extends Stmt // all expressions can be used as statements
case class FunCall(name: String, args: List[Expr]) extends Expr
case class Id(name: String) extends Expr
case class NumberLit(value: Int) extends Expr
case class Binary(op: String, left: Expr, right: Expr) extends Expr
case class Unary(op: String, arg: Expr) extends Expr