package ee.cyber.simplicitas.oberonexample

import ast._
import ee.cyber.simplicitas.SourceMessage

object NameBindingA4 extends NameBindingA2A {
//    def doField(env: Env)(f: FieldList) =
//        f.ids.ids.map(
//            (id: Id) => OField(id.text, typeValue(f.idType, env)))
//
//    // Convert parsed TypeValue to OType.
//    def typeValue(tv: TypeValue, env: Env): OType = tv match {
//        case id @ Id(_) =>
//            getType(id, env)
//        case RecordType(fields) =>
//            ORecord(fields.flatMap(doField(env)))
//        case ArrayType(expr, base) =>
//            OArray(typeValue(base, env))
//    }

    override def checkType(tv: TypeValue, env: EnvBase) {
        tv match {
            case id @ Id(name) =>
                env.checkType(id)
            case RecordType(fields) =>
                // TODO: check that all the Ids are unique.
                for (f <- fields) {
                    checkType(f.idType, env)
                }
            case ArrayType(expr, base) =>
                // TODO: typecheck expr
                checkType(base, env)
        }
    }
}