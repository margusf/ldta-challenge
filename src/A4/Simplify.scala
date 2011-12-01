package ee.cyber.simplicitas.oberonexample

import collection.mutable.ArrayBuffer
import ast._
import ee.cyber.simplicitas.{CommonNode, LiteralNode}

// Simplifies the Oberon program
// * Brings inner procedures to top level
// * Replaces case statements with series of if statements
object Simplify {
    def simplify(module: Module): Module = {
        val simpl = new Simplify(module)
        simpl()
    }
}

class Simplify(module: Module) {
    // List of top-level procedures (will be filled when procedure
    // declarations are flattened)
    val topLevel = ArrayBuffer[ProcedureDecl]()
    // For generating synthetic variable names.
    var currentId = 0
    // Mapping from procedure names to list of additional parameters
    // to the procedure
    val extraParams = collection.mutable.Map[Id, List[Id]]()

    def apply(): Module = {
        val ctx = new Ctx(getIds(module.decl), null)
        // As all the top-level variables are visible to procedures,
        // there is no need to actually process the ctx.freeVars
        doProcedures(ctx, module.decl.procedures)
        if (module.statements ne null) {
            module.statements.walkTree(processChild(ctx))
        }

        module.walkTree(fixProcedureCall)
        Module(module.name1,
            Declarations(
                module.decl.consts,
                module.decl.types,
                module.decl.vars ++ ctx.newVars,
                topLevel.toList),
            module.statements,
            module.name2)
    }

    private def getIds(decl: Declarations) = {
        val consts = decl.consts.map(_.name)
        val vars = decl.vars.flatMap(_.vars.ids)

        (consts ++ vars).toSet
    }

    private def getIds(params: List[FormalParam]) =
        if (params eq null)
            Set.empty
        else
            params.flatMap(_.ids.ids).toSet

    private def fixProcedureCall(node: CommonNode) {
        node match {
            case call @ ProcedureCall(name, args) =>
                val key = name.ref.asInstanceOf[Id]
                if (extraParams.contains(key)) {
                    if (args eq null) {
                        call.args = Nil
                    }
                    call.args ++= extraParams(key)
                }
            case _ =>
                ()
        }
    }

    private class Ctx(val globals: Set[Id], val parent: String) {
        /** Variables introduced by transformations within statements. */
        val newVars = ArrayBuffer[VarDef]()
        /** Free variables that are used by this procedure. */
        var freeVars = collection.mutable.Set[Id]()

        def getFullName(lastPart: String) =
            if (parent eq null)
                lastPart
            else
                parent + "_" + lastPart
    }

    private def doStatementSequence(ctx: Ctx, stmt: StatementSequence) {
        if ((stmt ne null) && (stmt.stmt ne null)) {
            stmt.stmt = stmt.stmt.foldRight[List[Statement]](Nil)(doStmt(ctx))
        }
    }

    private def caseClause(id: Id)(clause: CaseClause) = {
        def doConst(c: CaseConstant) =
            if (c.end ne null)
                Binary(BinaryOp.And,
                    Binary(BinaryOp.GreaterEqual, newId(id), c.begin),
                    Binary(BinaryOp.LessEqual, newId(id), c.end))
            else
                Binary(BinaryOp.Equals, newId(id), c.begin)

        // List of comparison operators
        val exprList = clause.items.map(doConst)
        // Or the operators together.
        val expr = exprList.reduceLeft(Binary(BinaryOp.Or, _, _))

        (expr, clause.stmt)
    }

    private def doStmt(ctx: Ctx)(stmt: Statement, old: List[Statement]):
            List[Statement] = {
        stmt match {
            case CaseStatement(expr, clauses, elseClause) =>
                // Artificial variable for case expression
                val exprVar = Id(newId)
                exprVar.exprType = Types.int
                exprVar.parent = expr.parent

                val exprDef = VarDef(
                    IdentList(List(exprVar)),
                    Id("INTEGER"))

                val ifClauses = clauses.map(caseClause(exprVar))
                val ifConds = ifClauses.map(_._1)
                val ifStatements = ifClauses.map(_._2)

                val ifStmt = IfStatement(
                    ifConds,
                    ifStatements,
                    elseClause)
                ctx.newVars += exprDef
                Assignment(newId(exprVar), expr) :: ifStmt :: old
            case _ =>
                // No direct transformation needed. The children were already
                // transformed.
                stmt :: old
        }
    }

    def getType(id: Id) = {
        val parent = (if (id.ref eq null) id else id.ref).parent

        parent match {
            case ConstantDef(_, _, expr) =>
                expr.exprType match {
                    case OInt() => Id("INTEGER")
                    case OBool() => Id("BOOLEAN")
                    case _ =>
                        throw new Exception("Invalid type: " +
                            expr.exprType)
                }
            case idList @ IdentList(_) =>
                idList.parent match {
                    case VarDef(_, vt) => vt
                    case FormalParam(_, _, pt) => pt
                    case _ => throw new Exception("Invalid parent: " +
                            idList.parent)
                }
            case _ =>
                throw new Exception("Invalid parent for " + id.text +
                        ": " + parent)
        }
    }

    private def processChild(ctx: Ctx)(child: CommonNode) {
        child match {
            case id: Id if (!id.exprType.isInstanceOf[ONonData]) =>
                ctx.freeVars += id
            case id: Id =>
            case s: StatementSequence =>
                doStatementSequence(ctx, s)
            case _ =>
                // Do nothing
        }
    }

    private def doProcedures(ctx: Ctx, procList: List[ProcedureDecl]) {
        for (proc <- procList) {
            // For flattening, change the name, but leave the Id object
            // unchanged.
            proc.name.text = ctx.getFullName(proc.name.text)

            val subCtx = new Ctx(ctx.globals, proc.name.text)
            doProcedures(subCtx, proc.decl.procedures)
            val bodyCtx = new Ctx(ctx.globals, proc.name.text)
            proc.body.walkTree(processChild(bodyCtx))

            val myVars = getIds(proc.decl) ++ getIds(proc.params) ++
                    bodyCtx.newVars.flatMap(_.vars.ids)
            val deltaVars = bodyCtx.freeVars ++ subCtx.freeVars --
                    myVars -- preDefs
            val deltaVarList = deltaVars.toList

            ctx.freeVars ++= deltaVars

            val newParams = deltaVarList.map((id: Id) =>
                FormalParam(
                    (if (id.isByRef)
                        LiteralNode("VAR")
                    else
                        null),
                    IdentList(List(id)), // xxx: ideally we should make copy and update refs
                    getType(id)))

            extraParams(proc.name) = deltaVarList

            topLevel += ProcedureDecl(
                    proc.name,
                    (if (proc.params eq null) Nil else proc.params) ++
                            newParams,
                    Declarations(
                            proc.decl.consts,
                            proc.decl.types,
                            proc.decl.vars ++ bodyCtx.newVars,
                            Nil),
                    proc.body,
                    proc.name2)
        }
    }

    private def newId = {
        currentId += 1
        "gen_" + currentId
    }

    /** Makes new Id that references the old Id. */
    private def newId(old: Id) = {
        val ret = Id(old.text)
        ret.ref = old.ref
        ret.byRef = old.byRef
        ret
    }

    private val preDefs = EnvA2A.preDefs.keys.map(Id(_))
}