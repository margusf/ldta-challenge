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


object OberonExtras {
    type WithText = {
        def text: String
    }

    def makeBinary(foo: WithText, left: Expression, right: Expression) =
        if (right eq null)
            left
        else
            Binary(BinaryOp.withName(foo.text), left, right)
}