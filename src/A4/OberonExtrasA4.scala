package ee.cyber.simplicitas.oberonexample.ast

case class ArrayAccess(array: Expression, index: Expression)
        extends Expression {
    def childrenNames = Array("array", "index")
}

case class RecordAccess(record: Expression, field: Id)
        extends Expression {
    def childrenNames = Array("record", "field")
}

object OberonExtrasA4 extends OberonExtras {
    def makeSelector(id: Expression, selectors: List[SelectorPart]) = {
        def doSelector(body: Expression, selector: SelectorPart) =
            selector match {
                case RecordSelector(field) =>
                    RecordAccess(body, field).setStart(body).setEnd(field)
                case ArraySelector(index) =>
                    ArrayAccess(body, index).setStart(body).setEnd(index)
            }

        selectors.foldLeft(id)(doSelector)
    }
}