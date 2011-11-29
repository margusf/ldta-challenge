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
                val index = evalConstExpr(expr)
                if (index < 0) {
                    throw new TypeError(expr, "Invalid array size: " + index)
                }
                OArray(typeValue(base, env))
            case _ =>
                super.typeValue(tv, env)
        }

    override def canBeLhs(expr: Expression) = expr match {
        case ArrayAccess(_, _) | RecordAccess(_, _) => true
        case _ => super.canBeLhs(expr)
    }

    override def canBeProcParam(t: TypeValue) = t match {
        case RecordType(_) | ArrayType(_, _) => false
        case _ => super.canBeProcParam(t)
    }

    override def canBeByValParam(t: OType) = t match {
        case ORecord(_) | OArray(_) => false
        case _ => super.canBeByValParam(t)
    }

    override protected def processExpr(expr: Expression): OType = {
        def arrayBase(t: OType) = t match {
            case OArray(base) =>
                base
            case _ =>
                throw new TypeError(expr, "Not an array: " + t)
        }

        def recordField(r: OType, f: String) = r match {
            case ORecord(fields) =>
                fields.find(_.name == f) match {
                    case Some(OField(_, fType)) => fType
                    case None =>
                        throw new TypeError(expr, "Unknown field: " + f)
                }
            case _ =>
                throw new TypeError(expr, "Not a record: " + r)
        }

        val retType = expr match {
            case ArrayAccess(id @ Id(_), index) =>
                checkInteger(index)
                arrayBase(id.ref.asInstanceOf[Id].exprType.asInstanceOf[OType])
            case ArrayAccess(array, index) =>
                checkInteger(index)
                arrayBase(processExpr(array))
            case RecordAccess(id @ Id(_), Id(field)) =>
                recordField(
                    id.ref.asInstanceOf[Id].exprType.asInstanceOf[OType],
                    field)
            case RecordAccess(record, Id(field)) =>
                recordField(processExpr(record), field)
            case _ =>
                super.processExpr(expr)
        }
        expr.exprType = retType
        retType
    }
}