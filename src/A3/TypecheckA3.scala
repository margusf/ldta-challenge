/*
 * // Copyright (c) 2010 Cybernetica AS / STACC
 */

package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, SourceLocation, SourceMessage}

object TypecheckA3 {
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
    override def processStatement(stm: Statement) {
        stm match {
            case ProcedureCall(name, args) =>
                // TODO: process procedure call
//                env.checkProc(name)
//                args.foreach(processExpr(_, env))
            case other =>
                super.processStatement(other)
        }
    }

    override def processDeclarations(decl: Declarations,
            env: EnvA2B): EnvA2B = {
        // TODO: eliminate copypaste.

        var newEnv = env

        for (td <- decl.types) {
            newEnv = newEnv.addType(td.name.text, typeValue(td.tValue, newEnv))
        }

        for (cd <- decl.consts) {
            val cv = evalConstExpr(cd.expr)
            cd.name.exprType = Types.int
            cd.name.constVal = Some(cv)
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

    def processProcedureDecl(pd: ProcedureDecl, env: EnvA2B) {
        // TODO: process parameters.
        for (fp <- pd.params;
                if fp ne null;
                id <- fp.ids.ids) {
//            id.byRef = fp.pVar ne null
//            if (fp.pVar ne null)
//                println("by ref variable: " + id.text)
            val paramType = typeValue(fp.pType, env)

            // ...
        }
        processDeclarations(pd.decl, env)

        processStatements(pd.body)
    }

    // TODO: add stuff
    override def typeValue(tv: TypeValue, env: EnvA2B): OType = tv match {
        case id @ Id(_) =>
            getType(id, env)
    }
}
