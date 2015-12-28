package org.uqbar.thin.more.views.source

import scala.language.existentials
import scala.language.implicitConversions
import scala.util.Try

import org.uqbar.utils.collections.immutable.IdentityMap

abstract class Encoder[-T](implicit grammarPreferences: GrammarPreferences) {
	implicit def StringToEncoderResult(s: String) = EncoderResult(s)

	def preferences = grammarPreferences.encodingPreferences
	def terminals = grammarPreferences.terminals

	def apply(target: T, level: Int = 0) = for {
		content <- _encode(target, level + preferences.tabulationLevelIncrement(On(this) on target))
	} yield formated(content referencing target, target)

	protected def formated(result: EncoderResult, target: T) =
		preferences.lineBreak(After(this) on target) ++
			preferences.space(Before(this) on target) ++
			result ++
			preferences.lineBreak(After(this) on target) ++
			preferences.space(After(this) on target)

	protected def tabulate(target: EncoderResult, level: Int): EncoderResult = preferences.tabulation(level) ++ target

	protected def _encode(target: T, level: Int): Try[EncoderResult]

}

class EmptyEncoder()(implicit grammarPreferences: GrammarPreferences) extends Encoder[Any] {
	protected def _encode(target: Any, level: Int) = Try("")
}

class ValueEncoder()(implicit grammarPreferences: GrammarPreferences) extends Encoder[Any] {
	protected def _encode(target: Any, level: Int) = Try(tabulate(target.toString, level))
}

class TerminalEncoder(terminal: Symbol)(implicit grammarPreferences: GrammarPreferences) extends Encoder[Any] {
	protected def _encode(target: Any, level: Int) = Try(tabulate(terminals(terminal), level))
}

class AppendEncoder[T, S](left: => Encoder[T], right: => Encoder[S])(implicit grammarPreferences: GrammarPreferences) extends Encoder[(T, S)] {
	protected def _encode(target: (T, S), level: Int) = for {
		previous <- left(target._1, level)
		next <- right(target._2)
	} yield previous ++ next
}

class TransformEncoder[T, S](before: => Encoder[S])(f: T => S)(implicit grammarPreferences: GrammarPreferences) extends Encoder[T] {
	protected def _encode(target: T, level: Int) = for {
		next <- before(f(target), level)
	} yield next
}

class OrEncoder[T, -L <: T, -R <: T](left: => Encoder[L], right: => Encoder[R])(implicit grammarPreferences: GrammarPreferences) extends Encoder[T] {
	protected def _encode(target: T, level: Int) =
		Try(target.asInstanceOf[L]).flatMap{ left(_, level) } orElse Try(target.asInstanceOf[R]).flatMap{ right(_, level) }
}

class RepeatEncoder[-T](body: => Encoder[T], separator: => Encoder[Null])(implicit grammarPreferences: GrammarPreferences) extends Encoder[Seq[T]] {
	protected def _encode(target: Seq[T], level: Int) = {
		val sortedTarget = preferences.sortOrder(this).fold(target){ target.sortWith(_) }
		if (sortedTarget.isEmpty) Try("")
		else ((body(sortedTarget.head, level), sortedTarget.head) /: sortedTarget.tail){
			case ((previous, previousElem), elem) =>
				(for {
					previous <- previous
					separator <- separator(null)
					elemBody <- body(elem, level)
				} yield previous ++ separator.dropReferences ++ preferences.lineBreak(InBetween(this) on (previousElem, elem, sortedTarget)) ++ elemBody, elem)
		}._1
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// ENCODER RESULTS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class EncoderResult(text: String = "", references: IdentityMap[Any, Range] = IdentityMap()) {
	implicit class ExtendedIdentityMap(m: IdentityMap[Any, Range]) {
	}

	def ++(text: String): EncoderResult = this ++ EncoderResult(text)
	def ++(other: EncoderResult) = {
		def shifted(m: IdentityMap[Any, Range], n: Int): IdentityMap[Any, Range] = m.map{ case (k, v) => (k, if (v.isInclusive) v.start + n to v.end + n else v.start + n until v.end + n) }

		EncoderResult(text + other.text, shifted(other.references, text.size) ++ references)
	}

	def referencing(target: Any) = {
		def fillingCount(s: Iterator[_]) = s.takeWhile(" \t\n".contains(_)).size

		val start = fillingCount(text.iterator)
		val end = text.size - fillingCount(text.reverseIterator)

		copy(references = references + (target, start until end))
	}

	def dropReferences = EncoderResult(text)
}