package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.CommonNode

// TODO: env should also map to kind (var, const, proc) and type.
class Env(parent: Env, defs: Map[String, Tuple2[CommonNode, OType]]) {
    def addProc(id: Id, params: List[OType]) =
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

    override def toString = defs.toString + " ==> " + parent
}

object Env {
    val predefs = Map[String, Tuple2[CommonNode, OType]](
        "Write" -> (null, OProc(List(OAny()))),
        "WriteLn" -> (null, OProc(Nil)),
        "Read" -> (null, OProc(List(OAny()))))

    def initialEnv =
        new Env(null, predefs) {
            override def get(name: String) = predefs.get(name)
            override def toString = "()"
        }
}
