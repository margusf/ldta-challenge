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

case class OArray(base: OType) extends OType {
    def assignableFrom(other: OType) = other match {
        case OArray(otherBase) =>
            base.assignableFrom(otherBase)
        case _ =>
            false
    }
}

case class ORecord(fields: Seq[OField]) extends OType {
    private def fieldAssignable(f: (OField, OField)) = {
        val (my, other) = f
        my.name == other.name && my.fType.assignableFrom(other.fType)
    }
    def assignableFrom(other: OType) = other match {
        case ORecord(oFields) =>
            fields.length == oFields.length &&
                fields.zip(oFields).forall(fieldAssignable)
        case _ =>
            false
    }
}

case class OField(name: String, fType: OType)

// Reference to predefined type alias. For use in C code generation.
case class ORef(ref: String) extends OType {
    // Will not be called.
    def assignableFrom(o: OType): Boolean = throw new Exception()
}