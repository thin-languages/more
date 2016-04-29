package org.uqbar.thin.more.views.source

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.util.Try
import org.uqbar.thin.more.DecodeException

object SourceDecoders extends RegexParsers {
	
	sealed abstract class SourceDecoder[+T] {
		lazy val inner = _inner
		protected def _inner: Parser[T]
		def apply(input: String) = parseAll(inner, input) match {
			case Success(r, _) => Try(r)
			case failure @ NoSuccess(_, _) => Try { throw new DecodeException(failure.toString) }
		}
	}

	//TODO: make _inner a lazy field of SourceDecoder (when tests are done) 
	object EmptyDecoder extends SourceDecoder[Null] { def _inner = "".r ^^^ null }
	class LexemeDecoder(restriction: Regex) extends SourceDecoder[String] { def _inner = restriction }
	class ConstantDecoder(key: Symbol)(implicit preferences: SourceViewPreferences) extends SourceDecoder[String] { def _inner = if (preferences.constants(key) == " ") "".r else preferences.constants(key) } //TODO: Find better way to handle the space as parseable value
	class AppendDecoder[T, S](left: => SourceDecoder[T], right: => SourceDecoder[S]) extends SourceDecoder[(T, S)] { def _inner = left.inner ~ right.inner ^^ { case l ~ r => (l, r) } }
	class TransformDecoder[T, U](target: => SourceDecoder[T])(tx: T => U) extends SourceDecoder[U] { def _inner = target.inner ^^ tx }
	class OrDecoder[T <: V, S <: V, V](left: => SourceDecoder[T], right: => SourceDecoder[S]) extends SourceDecoder[V] { def _inner = left.inner | right.inner }
	class RepeatDecoder[T](body: SourceDecoder[T], separator: SourceDecoder[Any]) extends SourceDecoder[List[T]] { def _inner = repsep(body.inner, separator.inner) }
}