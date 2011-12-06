package ee.cyber.simplicitas.oberonexample.gen

import ee.cyber.simplicitas.oberonexample.OType

abstract class Gen {
    def gen(buf: StringBuilder) {}
}

case class Module(name: String,
                  globals: List[Stmt],
                  procedures: List[ProcDecl],
                  statements: Stmt)

case class VarDecl(name: String, vType: OType) extends Stmt
case class ProcDecl(name: String,
                    args: List[Arg],
                    body: List[Stmt])
case class Arg(name: String, argType: OType)

// Something that can occur as post-condition in for statement
sealed trait ForPost
abstract class Stmt extends Gen
case class Nop() extends Stmt
case class Sequence(items: List[Stmt]) extends Stmt
case class Assign(lhs: Expr, value: Expr) extends Stmt
case class If(cond: Expr, ifStmt: Stmt, elseStmt: Stmt) extends Stmt
case class While(cond: Expr, body: Stmt) extends Stmt
case class For(pre: Stmt, cond: Expr, post: ForPost, body: Stmt) extends Stmt
case class Typedef(name: String, typeValue: OType) extends Stmt

case class Inc(id: String, value: Expr) extends Stmt with ForPost
case class Dec(id: String, value: Expr) extends Stmt with ForPost

abstract class Expr extends Stmt // all expressions can be used as statements
case class FunCall(name: String, args: List[Expr]) extends Expr
case class Id(name: String, isRef: Boolean) extends Expr
case class NumberLit(value: Int) extends Expr
case class Binary(op: String, left: Expr, right: Expr) extends Expr
case class Unary(op: String, arg: Expr) extends Expr
case class ArrayAccess(array: Expr, index: Expr) extends Expr
case class RecordAccess(record: Expr, field: String) extends Expr