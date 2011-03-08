package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.CommonNode

// TODO: env should also map to kind (var, const, proc) and type.
class Env(parent: Env, defs: Map[String, Tuple2[CommonNode, OType]]) {
    def addProc(id: Id, params: Seq[OType]) =
        new Env(this, Map(id.text -> (id, OProc(params))))

    def addPrimitive(id: Id, ptype: OType) =
        new Env(this, Map(id.text -> (id, ptype)))

    def addConst(id: Id, ptype: OType) =
        new Env(this, Map(id.text -> (id, ptype)))

    def get(name: String): Option[Tuple2[CommonNode, OType]] =
        if (defs.contains(name))
            Some(defs(name))
        else
            parent.get(name)

    def getFun(name: String): Option[OFunc] =
        get(name) match {
            case Some((_, x: OFunc)) => Some(x)
            case Some(x) => None // TODO: report that it is not function
            case None => None
        }

    override def toString = defs.toString + " ==> " + parent
}

object Env {
    import Types._

    def proc(name: String, params: OType*) =
        (name, (null, OProc(params)))

    def fun(name: String, ret: OType, params: OType*) =
        (name, (null, OFunc(params, ret)))

    val predefs = Map[String, Tuple2[CommonNode, OType]](
        proc("Write", any),
        proc("WriteLn"),
        proc("Read", any),

        fun("+", int, int, int),
        fun("-", int, int, int),
        fun("*", int, int, int),
        fun("DIV", int, int, int),
        fun("MOD", int, int, int),

        fun("<", bool, int, int),
        fun(">", bool, int, int),
        fun("<=", bool, int, int),
        fun(">=", bool, int, int),
        fun("=", bool, int, int),
        fun("#", bool, int, int),

        fun("&", bool, bool, bool),
        fun("OR", bool, bool, bool)
    )

    def initialEnv =
        new Env(null, predefs) {
            override def get(name: String) = predefs.get(name)
            override def toString = "()"
        }
}
