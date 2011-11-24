package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, SourceLocation, SourceMessage}

object TypecheckA4 {
    EnvA2A.Write.exprType = OProc(List((Types.int, ProcParamType.byValue)))
    EnvA2A.WriteLn.exprType = OProc(Nil)
    EnvA2A.Read.exprType = OProc(List((Types.int, ProcParamType.byRef)))

    def process(module: Module): Option[SourceMessage] = {
        try {
            val checker = new TypecheckA4
            checker.process(module)
            None
        } catch {
            case TypeError(loc, msg) =>
                Some(new SourceMessage(
                    msg, SourceMessage.Error, loc))
        }
    }
}

class TypecheckA4 extends TypecheckA3 {
    private def doField(env: EnvA2B)(f: FieldList) =
        f.ids.ids.map(
            (id: Id) => OField(id.text, typeValue(f.idType, env)))

    override protected def typeValue(tv: TypeValue, env: EnvA2B): OType =
        tv match {
            case RecordType(fields) =>
                ORecord(fields.flatMap(doField(env)))
            case ArrayType(expr, base) =>
                OArray(typeValue(base, env))
            case _ =>
                super.typeValue(tv, env)
        }

    override protected def processExpr(expr: Expression): OType = {
        val retType = expr match {
            case ArrayAccess(array, index) =>
                // TODO
                Types.int
            case RecordAccess(record, field) =>
                // TODO
                Types.int
            case _ =>
                super.processExpr(expr)
        }
        expr.exprType = retType
        retType
    }
}