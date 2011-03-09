package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.CommonNode

class Env(parent: Env,
          defs: Map[String, Tuple2[CommonNode, OType]],
          types: Map[String, OType]) {
    def addProc(id: Id, params: Seq[OType]) =
        new Env(this, Map(id.text -> (id, OProc(params))), Map.empty)

    def addPrimitive(id: Id, ptype: OType) =
        new Env(this, Map(id.text -> (id, ptype)), Map.empty)

    def addConst(id: Id, ptype: OType) =
        new Env(this, Map(id.text -> (id, ptype)), Map.empty)

    def addType(name: String, typeVal: OType) =
        new Env(this, Map.empty, Map(name -> typeVal))

    def get(name: String): Option[Tuple2[CommonNode, OType]] =
        if (defs.contains(name))
            Some(defs(name))
        else
            parent.get(name)

    def getFun(name: String): Option[OFunc] =
        get(name) match {
            case Some((_, x: OFunc)) => Some(x)
            case Some(x) => None
            case None => None
        }

    def getType(name: String): Option[OType] =
        if (types.contains(name))
            Some(types(name))
        else
            parent.getType(name)


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

    val preTypes = Map[String, OType](
        "INTEGER" -> Types.int,
        "BOOLEAN" -> Types.bool
    )

    def initialEnv =
        new Env(null, predefs, preTypes) {
            override def get(name: String) = predefs.get(name)
            override def getType(name: String) = preTypes.get(name)
            override def toString = "()"
        }
}
