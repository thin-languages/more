package org.uqbar.thin.more.views.source

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.uqbar.thin.more.views.source.SourceEncoders.EmptyEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.SourceEncoder
import org.scalatest.matchers.Matcher
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.io.StringWriter
import java.io.PrintWriter
import org.scalatest.matchers.MatchResult
import org.uqbar.thin.more.views.source.SourceEncoders.LexemeEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.ConstantEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.AppendEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.TransformEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.OrEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.RepeatEncoder

class SourceEncodersTests extends FreeSpec with Matchers {

	"Source Encoders" - {
		"should encode properly" - {

			implicit val preferences = SourceViewPreferences(Map('x -> "X"), FormattingPreferences())

			"EmptyEncoder" in {
				implicit val encoder = new EmptyEncoder()

				Nil should beEncodedTo ("") ()
			}
			
			"LexemeEncoder" in {
				implicit val encoder = new LexemeEncoder()
				
				Nil should beEncodedTo ("List()") (Nil -> (0 to 5))
			}
			
			"ConstantEncoder" in {
				implicit val encoder = new ConstantEncoder('x)
				
				Nil should beEncodedTo ("X") (Nil -> (0 to 0))
			}
			
			"AppendEncoder" in {
				implicit val encoder = new AppendEncoder(new AppendEncoder(new ConstantEncoder('x), new LexemeEncoder()), new LexemeEncoder())
				
				((Nil, "Y"), 'Z') should beEncodedTo ("XYZ") (Nil -> (0 to 0), "Y" -> (1 to 1), 'Z' -> (2 to 2))
			}
			
			"TransformEncoder" in {
				implicit val encoder = new TransformEncoder(new LexemeEncoder())((_:List[_]).size)
				
				Nil should beEncodedTo ("0") (Nil -> (0 to 0), 0 -> (0 to 0))
			}
			
			"OrEncoder" in {
				trait T
				case class X(s: String) extends T
				case class Y(n: Int) extends T
				
				val x = X("foo")
				val y = Y(5)
				
				val XEncoder = new TransformEncoder(new LexemeEncoder())((_:X).s)
				val YEncoder = new TransformEncoder(new LexemeEncoder())((_:Y).n)
				implicit val encoder = new OrEncoder(XEncoder,YEncoder)
				
				x should beEncodedTo ("foo") (x -> (0 to 2), "foo" -> (0 to 2))
				y should beEncodedTo ("5") (y -> (0 to 0), 5  -> (0 to 0))
			}
			"RepeatEncoder" in {
				
				implicit val encoder = new RepeatEncoder(new LexemeEncoder(), new ConstantEncoder('x))
				
				val list1 = List(1)
				val list2 = List(1,2)
				val list3 = List(1,2,3)
				
				Nil should beEncodedTo ("") (Nil -> (0 until 0) )
				list1 should beEncodedTo ("1") (list1 -> (0 to 0), 1 -> (0 to 0) )
				list2 should beEncodedTo ("1X2") (list2 -> (0 to 2), 1 -> (0 to 0), 2 -> (2 to 2) )
				list3 should beEncodedTo ("1X2X3") (list3 -> (0 to 4), 1 -> (0 to 0), 2 -> (2 to 2), 3 -> (4 to 4) )
			}
		}
	}

}

case class beEncodedTo[T](tabulatedExpectedText: String)(expectedReferencesSeq: (Any, Range)*)(implicit encoder: SourceEncoder[T], preferences: SourceViewPreferences) extends Matcher[T] {
	protected implicit class TesteableSource(target: Try[Source]) {
		def matches(tabulatedExpectedText: String, references: Map[Any, Range], tabulatedTags: Map[Any, Int] = Map()) = {
			val (expectedText, tags) = trimUnwantedTabulation(tabulatedExpectedText, tabulatedTags)
			val expectedReferences = if (tags.nonEmpty) references.mapValues { range => if (range.isInclusive) tags(range.start) to tags(range.end) else tags(range.start) until tags(range.end) } else references

			val (success, message) = target match {
				case Success(Source(text, _)) if text != expectedText => false -> s"Encoded text: ${pretty(text)} did not match expected text: ${pretty(expectedText)}"
				case Success(Source(text, references)) =>
					val unexpectedReferences = references.filterNot(expectedReferences isDefinedAt _._1)
					val missedReferences = expectedReferences.filterNot(references isDefinedAt _._1)
					val wrongReferences = references.filter{ case (key, value) => expectedReferences.get(key).exists(_ != value) }
					if (unexpectedReferences.nonEmpty) false -> s"Encoding yielded references to unexpected objects: ${pretty(unexpectedReferences, text)}"
					else if (missedReferences.nonEmpty) false -> s"Encoding didn't yield references to expected objects: ${pretty(missedReferences, text)}"
					else if (wrongReferences.nonEmpty) false -> s"Encoding yielded wrong references: ${pretty(wrongReferences, expectedReferences, text)}"
					else true -> "Encoded was as expected"
				case Failure(e) =>
					val stack = new StringWriter
					e.printStackTrace(new PrintWriter(stack))
					false -> s"Encoding failed because of $e: ${stack.toString}}"
			}
			MatchResult(success, message, message)
		}

		protected def trimUnwantedTabulation(tabulatedText: String, tabulatedTags: Map[Any, Int]) = {
			val unwantedTabulation :: _ = """^\n?([\t| ]*)[^\$]*""".r.unapplySeq(tabulatedText).get

			def dropLeadingEmptyLine(lines: Seq[String], tags: Map[Any, Int]) =
				if (lines.nonEmpty && lines.head.isEmpty)
					lines.tail -> tags.map{ case (k, v) => (k, v - 1) }
				else lines -> tags

			def dropUnwantedTabulation(lines: Seq[String], tags: Map[Any, Int]) = {
				val (trimmedLines, trimmedTags, _) = ((List[String](), tags, -1) /: lines){
					case ((trimmedLines, tags, lastBreakIndex), part) =>
						val nextBreakIndex = lastBreakIndex + part.size + 1
						val nextTrimmedLines = trimmedLines :+ part.replaceFirst(unwantedTabulation, "")
						val nextTags = tags.map {
							case (k, v) if lastBreakIndex to nextBreakIndex contains v => (k, v - unwantedTabulation.size * (lines.take(trimmedLines.size).filter(_.startsWith(unwantedTabulation)).size + 1))
							case other => other
						}
						(nextTrimmedLines, nextTags, nextBreakIndex)
				}

				(trimmedLines, trimmedTags)
			}

			def dropTrailingEmptyLine(lines: Seq[String], tags: Map[Any, Int]) =
				if (lines.nonEmpty && lines.last.forall(" \t" contains _))
					lines.init -> tags
				else lines -> tags

			val (lines, tags) = (
				(dropLeadingEmptyLine _).tupled andThen
				(dropUnwantedTabulation _).tupled andThen
				(dropTrailingEmptyLine _).tupled
			)(tabulatedText.split("\n"), tabulatedTags)

			(lines.mkString("\n"), tags)
		}

		protected def pretty(text: String): String = s""""${text.replaceAll(" ", "·").replaceAll("\t", "»").replaceAll("\n", "¶")}""""
		protected def pretty(range: Range, text: String): String = s"[${range.start}, ${range.end}${if (range.isInclusive) "]" else ")"}: ${pretty(text.substring(range.start, if (range.isInclusive) range.end + 1 else range.end))}"
		protected def pretty(references: Map[Any, Range], text: String): String = references.map(r => s"${r._1}: ${pretty(r._2, text)}").mkString("{", ", ", "}")
		protected def pretty(references: Map[Any, Range], expectedReferences: Map[Any, Range], text: String): String = references.map{ r => s"${r._1} -> ${pretty(r._2, text)} wasn't ${pretty(expectedReferences(r._1), text)}" }.mkString("{", ", ", "}")
	}

	def apply(target: T) = encoder(target).matches(tabulatedExpectedText, expectedReferencesSeq.toMap)
}