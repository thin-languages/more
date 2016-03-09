package org.uqbar.thin.more.views.source

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.util.Try

trait Parsers extends RegexParsers {
	abstract class CodeParser[+T] {
		lazy val inner = _inner
		protected def _inner: Parser[T]
		def apply(input: String) = parseAll(inner, input) match {
			case Success(r, _) => Try(r)
			case failure @ NoSuccess(_, _) => Try(throw new RuntimeException(failure.toString)) //TODO: Mejorar excepción
		}
	}

	object EmptyParser extends CodeParser[Null] { def _inner = "".r ^^^ null }
	
	class ValueParser(restriction: Regex) extends CodeParser[String] { def _inner = restriction }
	
	class TerminalParser(key: Symbol)(implicit options: GrammarPreferences) extends CodeParser[String] { def _inner = if (options.constants(key) == " ") "".r else options.constants(key) } //TODO: Find better way to handle the space as parseable value
	
	class AppendParser[T, S](left: => CodeParser[T], right: => CodeParser[S]) extends CodeParser[(T, S)] { def _inner = left.inner ~ right.inner ^^ { case l ~ r => (l, r) } }
	
	class TransformParser[T, U](target: => CodeParser[T])(tx: T => U) extends CodeParser[U] { def _inner = target.inner ^^ tx }
	
	class OrParser[T <: V, S <: V, V](left: => CodeParser[T], right: => CodeParser[S]) extends CodeParser[V] { def _inner = left.inner | right.inner }
	
	class RepeatParser[T, S](body: CodeParser[T], separator: CodeParser[S]) extends CodeParser[List[T]] { def _inner = repsep(body.inner, separator.inner) }
}