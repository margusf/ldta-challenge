package ee.cyber.simplicitas.prettyprint

import java.io.Writer

object Doc {
    val empty = Empty

    def char(c: Char) = c match {
        case '\n' => line
        case _ => DChar(c)
    }

    def text(s: String) = s match {
        case "" => empty
        case _ => Text(s)
    }

    val line = Line(false)
    val linebreak = Line(true)

    def beside(l: Doc, r: Doc) = Cat(l, r)
    def nest(indent: Int, doc: Doc) = Nest(indent, doc)
    def column(f: Int => Doc) = Column(f)
    def nesting(f: Int => Doc) = Nesting(f)
    def group(x: Doc) = Union(flatten(x), x)

    private def flatten(doc: Doc): Doc = doc match {
        case Cat(x, y) => Cat(flatten(x), flatten(y))
        case Nest(i, x) => Nest(i, flatten(x))
        case Line(break) => if (break) empty else Text(" ")
        case Union(x, y) => flatten(x)
        case Column(f) => Column(flatten _ compose f)
        case Nesting(f) => Nesting(flatten _  compose f)
    }

    // Renderers
    private def renderPretty(ribbonFrac: Double, width: Int,
                             doc: Doc): SimpleDoc = {
        //r :: the ribbon width in characters
        val r = 0 max (width min ((width * ribbonFrac) round).toInt)

        // nicest :: r = ribbon width, w = page width,
        //           n = indentation of current line, k = current column
        //           x and y, the (simple) documents to chose from.
        //           precondition: first lines of x are longer than the
        //           first lines of y.
        def nicest(n: Int, k: Int, x: SimpleDoc, y: SimpleDoc) = {
            val w = (width - k) min (r - k + n)

            if (fits(w, x))
                x
            else
                y
        }

        // best :: n = indentation of current line
        //         k = current column
        //         (ie. (k >= n) && (k - n == count of inserted characters)
        def best(n: Int, k: Int, d: Docs): SimpleDoc = d match {
            case DNil => SEmpty
            case DCons(i, d, ds) => d match {
                case Empty => best(n, k, ds)
                case DChar(c) => SChar(c, best(n, k + 1, ds))
                case Text(s) => SText(s, best(n, k + 1, ds))
                case Line(_) => SLine(i, best(i, i, ds))
                case Cat(x, y) => best(n, k, DCons(i, x, DCons(i, y, ds)))
                case Nest(j, x) => best(n, k, DCons(i + j, x, ds))
                case Union(x, y) =>
                    nicest(n, k,
                        best(n, k, DCons(i, x, ds)),
                        best(n, k, DCons(i, y, ds)))
                case Column(f) => best(n, k, DCons(i, f(k), ds))
                case Nesting(f) => best(n, k, DCons(i, f(i), ds))
            }
        }

        best(0, 0, DCons(0, doc, DNil))
    }

    private def fits(w: Int, x: SimpleDoc): Boolean =
        if (w < 0)
            false
        else x match {
            case SEmpty => true
            case SChar(c, x) => fits(w - 1, x)
            case SText(s, x) => fits(w - s.length, x)
            case SLine(_, _) => true
        }

    def show(doc: Doc, width: Int): String = {
        val writer = new java.io.StringWriter
        show(doc, 0.9, width, writer)
        writer.toString
    }

    def show(doc: Doc, rfrac: Double, width: Int, writer: Writer) {
        def display(d: SimpleDoc): Unit = d match {
            case SEmpty => ()
            case SChar(c, x) =>
                writer.write(c)
                display(x)
            case SText(s, x) =>
                writer.write(s)
                display(x)
            case SLine(i, x) =>
                writer.write('\n')
                spaces(i, writer)
                display(x)
        }

        val sdoc = renderPretty(rfrac, width, doc)
        display(sdoc)
    }

    private def spaces(n: Int, writer: Writer) {
        var rem = n
        while (rem >= 16) { writer write "                "; rem -= 16 }
        if (rem >= 8)     { writer write "        "; rem -= 8 }
        if (rem >= 4)     { writer write "    "; rem -= 4 }
        if (rem >= 2)     { writer write "  "; rem -= 2}
        if (rem == 1)     { writer write " " }
    }
}

abstract class Doc {
    override def toString = Doc.show(this, 70)
}
case object Empty extends Doc
case class DChar(c: Char) extends Doc
case class Text(s: String) extends Doc
case class Line(isHard: Boolean) extends Doc
case class Cat(l: Doc, r: Doc) extends Doc
case class Nest(indent: Int, doc: Doc) extends Doc
case class Union(x: Doc, y: Doc) extends Doc
case class Column(f: Int => Doc) extends Doc
case class Nesting(f: Int => Doc) extends Doc

abstract class SimpleDoc
case object SEmpty extends SimpleDoc
case class SChar(c: Char, doc: SimpleDoc) extends SimpleDoc
case class SText(s: String, doc: SimpleDoc) extends SimpleDoc
case class SLine(i: Int, doc: SimpleDoc) extends SimpleDoc

abstract class Docs
case object DNil extends Docs
case class DCons(i: Int, d: Doc, ds: Docs) extends Docs