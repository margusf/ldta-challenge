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
            val checker = new TypecheckA3
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
}