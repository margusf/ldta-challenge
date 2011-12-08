package ee.cyber.simplicitas.oberonexample

import ast._

object ConstantEval {
    def tryEvalConstExpr(expr: Expression): Option[Int] = {
        def checkIntFun(op: String) {
            EnvA2B.operators.get(op) match {
                case Some(OFunc(_, Types.int)) =>
                    // OK
                case _ =>
                    throw new TypeError(expr, "Int required: " + op)
            }
        }

        def evalBinary(op: String, left: Int, right: Int) = {
            checkIntFun(op)

            op match {
                case "+" => left + right
                case "-" => left - right
                case "*" => left * right
                case "DIV" =>
                    if (right == 0)
                        throw new TypeError(expr, "Division by zero")
                    else
                        left / right
                case "MOD" =>
                    if (right == 0)
                        throw new TypeError(expr, "Division by zero")
                    else
                        left % right

            }
        }

        def evalUnary(op: String, arg: Int) = {
            checkIntFun(op)

            op match {
                case "+" => arg
                case "-" => -arg
            }
        }

        expr match {
            case id @ Id(name) =>
                val ref = if (id.ref eq null) id else id.ref
                if (Types.int.assignableFrom(
                        ref.exprType.asInstanceOf[OType]))
                    ref.constVal
                else
                    None
            case Binary(op, left, right) =>
                (tryEvalConstExpr(left), tryEvalConstExpr(right)) match {
                    case (Some(l), Some(r)) =>
                        Some(evalBinary(op.toString, l, r))
                    case _ => None
                }
            case Unary(op, arg) =>
                tryEvalConstExpr(arg).map(evalUnary(op.toString, _))
            case NumberLit(v) =>
                Some(v.toInt)
            case _ =>
                None
        }
    }

    def evalConstExpr(expr: Expression): Int =
        tryEvalConstExpr(expr) match {
            case Some(value) => value
            case None =>
                throw new TypeError(expr, "Not a constant: " + expr)
        }
}
