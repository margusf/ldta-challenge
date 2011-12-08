package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.SourceMessage

object NameBindingA4 extends NameBindingA2A {
    override def checkType(tv: TypeValue, env: EnvBase) {
        tv match {
            case id @ Id(name) =>
                env.checkType(id)
            case RecordType(fields) =>
                for (f <- fields) {
                    checkType(f.idType, env)
                }
                checkDuplicates(
                    for (f <- fields; id <- f.ids.ids)
                        yield id)
            case ArrayType(expr, base) =>
                processExpr(expr, env)
                checkType(base, env)
        }
    }

    override def processExpr(expr: Expression, env: EnvBase) {
        expr match {
            case ArrayAccess(array, index) =>
                processExpr(array, env)
                processExpr(index, env)
            case RecordAccess(record, field) =>
                processExpr(record, env)
            case _ =>
                super.processExpr(expr, env)
        }
    }

    override def processStatement(stm: Statement, env: EnvBase) {
        stm match {
            case Assignment(lhs @ RecordAccess(_, _), rhs) =>
                processExpr(lhs, env)
                processExpr(rhs, env)
            case Assignment(lhs @ ArrayAccess(_, _), rhs) =>
                processExpr(lhs, env)
                processExpr(rhs, env)
            case _ =>
                super.processStatement(stm, env)
        }
    }
}
