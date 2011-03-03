package ee.cyber.simplicitas.oberonexample;

trait OType {
    def assignableFrom(other: OType): Boolean
}

object Types {
    val any = OAny()
    val bool = OBool()
    val int = OInt()
    val invalid = OInvalid()
}

case class OAny extends OType {
    def assignableFrom(other: OType) = true
    override def toString = "ANY"
}

case class OInvalid extends OType {
    def assignableFrom(other: OType) = true
}

case class OBool extends OType {
    def assignableFrom(other: OType) = other.isInstanceOf[OBool]
    override def toString = "BOOLEAN"
}

case class OInt extends OType {
    def assignableFrom(other: OType) = other.isInstanceOf[OInt]
    override def toString = "INTEGER"
}

case class OFunc(args: Seq[OType], ret: OType) extends OType {
    def assignableFrom(other: OType) = true
}

case class OProc(args: Seq[OType]) extends OType {
    def assignableFrom(other: OType) = true
}

// TODO: deal with constants and named types.
