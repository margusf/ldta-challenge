package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, SourceLocation, SourceMessage}

object TypecheckA3 {
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

class TypecheckA3 extends TypecheckA2B {
    def canBeByVarArg(expr: Expression) = expr match {
        // Non-const IDs are ok.
        case id @ Id(_) if id.ref.asInstanceOf[Id].constVal == None =>
            true
        case _ =>
            false
    }

    override def processStatement(stm: Statement) {
        stm match {
            case ProcedureCall(name, args) =>
                val proc = name.ref.asInstanceOf[Id]
                proc.exprType match {
                    case OProc(params) =>
                        if (args.length != params.length) {
                            throw new TypeError(name,
                                "Invalid parameter count: " + args.length +
                                        " instead of " + params.length)
                        }
                        for (((paramType, paramConst), a) <- params.zip(args)) {
                            val argType = processExpr(a)
                            checkType(paramType, argType, a)

                            // Oberon uses nominal typing.
                            if (argType ne paramType) {
                                throw new TypeError(a,
                                        "Composite types must have same names.")
                            }

                            if (paramConst == ProcParamType.byRef &&
                                    !canBeByVarArg(a)) {
                                throw new TypeError(a,
                                    "Cannot be passed as var: " + a)
                            }
                        }
                    case _ =>
                        throw new TypeError(name,
                            "Not a procedure: " + name.text)
                }
            case other =>
                super.processStatement(other)
        }
    }

    override def processDeclarations(decl: Declarations,
            env: EnvA2B): EnvA2B = {
        // TODO: eliminate copypaste.

        var newEnv = env

        for (cd <- decl.consts) {
            val cv = evalConstExpr(cd.expr)
            cd.name.exprType = Types.int
            cd.name.constVal = Some(cv)
        }

        for (td <- decl.types) {
            newEnv = newEnv.addType(td.name.text, typeValue(td.tValue, newEnv))
        }

        for (vd <- decl.vars; id <- vd.vars.ids) {
            id.exprType = typeValue(vd.varType, newEnv)
        }

        // New in A3
        for (pd <- decl.procedures) {
            processProcedureDecl(pd, newEnv)
        }

        newEnv
    }

    def canBeProcParam(t: TypeValue) = true

    def canBeByValParam(t: OType) = true

    def processProcedureDecl(pd: ProcedureDecl, env: EnvA2B) {
        val paramTypes = new ArrayBuffer[(OType, ProcParamType.Type)]
        for (fp <- pd.params;
                if fp ne null;
                id <- fp.ids.ids) {
            id.byRef = fp.pVar ne null
            val paramType = typeValue(fp.pType, env)
            id.exprType = paramType
            paramTypes += ((paramType,
                    if (id.byRef) ProcParamType.byRef
                    else ProcParamType.byValue))

            if (!canBeProcParam(fp.pType) ||
                    (!id.byRef && !canBeByValParam(paramType))) {
                throw new TypeError(fp.pType,
                    "Cannot be used as procedure parameter: " + fp.pType)
            }
        }
        pd.name.exprType = OProc(paramTypes.toList)

        processDeclarations(pd.decl, env)

        processStatements(pd.body)
    }
}
