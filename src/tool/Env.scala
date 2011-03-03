package ee.cyber.simplicitas.oberonexample

import ee.cyber.simplicitas.CommonNode

// TODO: env should also map to kind (var, const, proc) and type.
class Env(parent: Env, defs: Map[String, CommonNode]) {
    def add(name: String, value: CommonNode): Env = {
        println("add: " + name)
        new Env(this, Map(name -> value))
    }

    def add(id: Id): Env = add(id.text, id)

    def get(name: String): Option[CommonNode] =
        if (defs.contains(name))
            Some(defs(name))
        else
            parent.get(name)

    override def toString = defs.toString + " <- " + parent
}

object Env {
    val predefs = Map[String, CommonNode](
        "Write" -> null,
        "WriteLn" -> null,
        "Read" -> null)

    def initialEnv =
        new Env(null, predefs) {
            override def get(name: String) = predefs.get(name)
            override def toString = "()"
        }


}
