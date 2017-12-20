package amyc.utils

// A structured document to be printed with nice indentation
abstract class Document {

  def <:>(other: Document) = Lined(List(this, other))

  def print: String = {
    val sb = new StringBuffer()

    def rec(d: Document)(implicit ind: Int): Unit = d match {
      case Raw(s) =>
        sb append ("  " * ind)
        sb append s
      case Indented(doc) =>
        rec(doc)(ind + 1)
      case Unindented(doc) =>
        rec(doc)(ind - 1)
      case Lined(Nil, _) => // skip
      case Lined(docs, sep) =>
        sb append ("  " * ind)
        // Hack: We don't want to reprint the indentation, so we pass 0
        docs.init foreach { d =>
          rec(d)(0)
          rec(sep)(0)
        }
        rec(docs.last)(0)
      case Stacked(Nil, _) => // skip
      case Stacked(docs, emptyLines) =>
        docs.init foreach { d =>
          rec(d)
          sb append "\n"
          if (emptyLines) sb append "\n"
        }
        rec(docs.last)
    }

    rec(this)(0)
    sb.toString
  }
}
case class Indented(content: Document) extends Document
case class Unindented(content: Document) extends Document
case class Stacked(docs: List[Document], emptyLines: Boolean = false) extends Document
case class Lined(docs: List[Document], separator: Document = Raw("")) extends Document
case class Raw(s: String) extends Document

object Stacked {
  def apply(docs: Document*): Stacked = Stacked(docs.toList)
}