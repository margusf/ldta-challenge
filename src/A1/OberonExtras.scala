package ee.cyber.simplicitas.oberonexample.ast

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

    val precedence = Map[Any, Int](
        LessThan -> 1, LessEqual -> 1, GreaterThan -> 1, GreaterEqual -> 1,
            Equals -> 1, NotEquals -> 1,
        UnaryOp.Pos -> 2, UnaryOp.Neg -> 2,
        Plus -> 3, Minus -> 3, Or -> 3,
        Times -> 4, Div -> 4, Mod -> 4, And -> 4,
        UnaryOp.Not -> 5)
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
class OberonExtras {
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
}

object OberonExtras extends OberonExtras