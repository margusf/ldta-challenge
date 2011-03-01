package ee.cyber.simplicitas.oberonexample

case class Binary(op: BinaryOp.Type, left: Expression, right: Expression)
        extends Expression {
    def childrenNames = Array("op", "left", "right")
}

object BinaryOp extends Enumeration {
    type Type = Value

    val Plus = Value("+")
    val Minus = Value("-")
    val Times = Value("*")
    val Div = Value("DIV")
    val Mod = Value("MOD")

    val LessThan = Value("<")
    val LessEqual = Value("<=")
    val GreaterThan = Value(">")
    val GreaterEqual = Value(">=")
    val Equals = Value("=")
    val NotEquals = Value("#")

    val And = Value("&")
    val Or = Value("OR")
}

case class Unary(op: UnaryOp.Type, arg: Expression) extends Expression {
    def childrenNames = Array("op", "arg")
}

object UnaryOp extends Enumeration {
    type Type = Value

    val Pos = Value("+")
    val Neg = Value("-")
    val Not = Value("~")
}

case class ArrayAccess(array: Expression, index: Expression)
        extends Expression {
    def childrenNames = Array("array", "index")
}

case class RecordAccess(record: Expression, field: Id)
        extends Expression {
    def childrenNames = Array("record", "field")
}

object OberonExtras {
    type WithText = {
        def text: String
    }

    def makeBinary(foo: WithText, left: Expression, right: Expression) =
        if (right eq null)
            left
        else
            Binary(BinaryOp.withName(foo.text), left, right)

    def makeBinary(ops: List[WithText], args: List[Expression]) = {
        def loop(left: Expression, right: List[Expression],
                 ops: List[BinaryOp.Type]): Expression =
            (right, ops) match {
                case (rh :: rt, oh :: ot) =>
                    loop(
                        Binary(oh, left, rh).setStart(left).setEnd(rh),
                        rt, ot)
                case (Nil, Nil) =>
                    left
                case _ =>
                    throw new Exception("Cannot happen")
            }

        loop(args.head, args.tail,
            ops.map((op: WithText) => BinaryOp.withName(op.text)))
    }

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