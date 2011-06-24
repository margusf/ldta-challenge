// This is prettyprinting library from Scala with some modifications
// to better fit the task of code generation.

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


/*
SCALA LICENSE

Copyright (c) 2002-2011 EPFL, Lausanne, unless otherwise specified.
All rights reserved.

This software was developed by the Programming Methods Laboratory of the
Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.

Permission to use, copy, modify, and distribute this software in source
or binary form for any purpose with or without fee is hereby granted,
provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

   3. Neither the name of the EPFL nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.


THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
*/

package ee.cyber.simplicitas.prettyprint


import java.io.Writer

case object DocNil extends Document
case object DocBreak extends Document
case object DocSpace extends Document
case object DocLineBreak extends Document
case class DocText(txt: String) extends Document
case class DocGroup(doc: Document) extends Document
case class DocNest(indent: Int, doc: Document) extends Document
case class DocCons(hd: Document, tl: Document) extends Document

/**
 * A basic pretty-printing library, based on Lindig's strict version
 * of Wadler's adaptation of Hughes' pretty-printer.
 *
 * @author Michel Schinz
 * @version 1.0
 */
abstract class Document {
  def ::(hd: Document): Document = DocCons(hd, this)
  def ::(hd: String): Document = DocCons(DocText(hd), this)
  def :/:(hd: Document): Document = hd :: DocBreak :: this
  def :/:(hd: String): Document = hd :: DocBreak :: this

  /**
   * Format this document on <code>writer</code> and try to set line
   * breaks so that the result fits in <code>width</code> columns.
   *
   * @param width  ...
   * @param writer ...
   */
  def format(width: Int, writer: Writer) {
    type FmtState = (Int, Boolean, Document)

    def fits(w: Int, state: List[FmtState]): Boolean = state match {
      case _ if w < 0 =>
        false
      case List() =>
        true
      case (_, _, DocNil) :: z =>
        fits(w, z)
      case (i, b, DocCons(h, t)) :: z =>
        fits(w, (i,b,h) :: (i,b,t) :: z)
      case (_, _, DocText(t)) :: z =>
        fits(w - t.length(), z)
      case (i, b, DocNest(ii, d)) :: z =>
        fits(w, (i + ii, b, d) :: z)
      case (_, false, DocBreak) :: z =>
        fits(w, z)
      case (_, true, DocBreak) :: z =>
        true
      case (_, false, DocSpace) :: z =>
        fits(w - 1, z)
      case (_, true, DocSpace) :: z =>
        true
      case (_, _, DocLineBreak) :: z =>
        true
      case (i, _, DocGroup(d)) :: z =>
        fits(w, (i, false, d) :: z)
    }

    def spaces(n: Int) {
      var rem = n
      while (rem >= 16) { writer write "                "; rem -= 16 }
      if (rem >= 8)     { writer write "        "; rem -= 8 }
      if (rem >= 4)     { writer write "    "; rem -= 4 }
      if (rem >= 2)     { writer write "  "; rem -= 2}
      if (rem == 1)     { writer write " " }
    }

    def fmt(k: Int, state: List[FmtState]): Unit = state match {
      case List() => ()
      case (_, _, DocNil) :: z =>
        fmt(k, z)
      case (i, b, DocCons(h, t)) :: z =>
        fmt(k, (i, b, h) :: (i, b, t) :: z)
      case (i, _, DocText(t)) :: z =>
        writer write t
        fmt(k + t.length(), z)
      case (i, b, DocNest(ii, d)) :: z =>
        fmt(k, (i + ii, b, d) :: z)
      case (i, true, DocBreak) :: z =>
        writer write "\n"
        spaces(i);
        fmt(i, z)
      case (i, false, DocBreak) :: z =>
        fmt(k + 1, z)
      case (i, true, DocSpace) :: z =>
        writer write "\n"
        spaces(i);
        fmt(i, z)
      case (i, _, DocLineBreak) :: z =>
        writer write "\n"
        spaces(i)
        fmt(i, z)
      case (i, false, DocSpace) :: z =>
        writer write " "
        fmt(k + 1, z)
      case (i, b, DocGroup(d)) :: z =>
        val fitsFlat = fits(width - k, (i, false, d) :: z)
        fmt(k, (i, !fitsFlat, d) :: z)
    }

    fmt(0, (0, false, DocGroup(this)) :: Nil)
  }
}

object Document {
  /** The empty document */
  def empty = DocNil

  /** A soft line break. */
  def break = DocBreak

  /** A break, which will either be turned into a space or a line break */
  def space = DocSpace

  /** Line break. */
  def lineBreak = DocLineBreak

  /** A document consisting of some text literal */
  def text(s: String): Document = DocText(s)

  /**
   * A group, whose components will either be printed with all breaks
   * rendered as spaces, or with all breaks rendered as line breaks.
   */
  def group(d: Document): Document = DocGroup(d)

  /** A nested document, which will be indented as specified. */
  def nest(i: Int, d: Document): Document = DocNest(i, d)
}