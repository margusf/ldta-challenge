package ee.cyber.simplicitas.oberonexample;

trait OType;

case class OAny extends OType
case class OBoolean extends OType
case class OInteger extends OType

case class OFunc(args: List[OType], ret: OType) extends OType
case class OProc(args: List[OType]) extends OType

// TODO: deal with constants and named types.