package org.uqbar.thin.more.views.source

import org.uqbar.thin.more.LanguageConcept
import org.uqbar.thin.more.LanguageView
import org.uqbar.thin.more.views.source.GrammarDefinition.GrammarToDecoder
import org.uqbar.thin.more.views.source.GrammarDefinition.GrammarToEncoder
import org.uqbar.thin.more.views.source.SourceDecoders.SourceDecoder
import org.uqbar.thin.more.views.source.SourceEncoders.SourceEncoder
import org.uqbar.utils.collections.immutable.IdentityMap

import SourceDecoders.SourceDecoder
import SourceEncoders.SourceEncoder

object SourceView extends LanguageView[Source, SourceViewPreferences, Sourceable] {
	def encode(target: LanguageConcept)(implicit language: Sourceable, options: SourceViewPreferences) = {
		val encoder: SourceEncoder[LanguageConcept] = language.grammar(target)
		encoder(target)
	}
	def decode(target: Source)(implicit language: Sourceable, options: SourceViewPreferences) = {
		val decoder: SourceDecoder[LanguageConcept] = language.grammar(target)
		decoder.apply(target.text)
	}
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// VIEW REQUIREMENTS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait Sourceable extends GrammarSugar {
	def root: Grammar[LanguageConcept] = Fail

	def grammar(target: LanguageConcept): GrammarDefinition[LanguageConcept, _] = root
	def grammar(target: Source): GrammarDefinition[_, LanguageConcept] = root
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// VIEW MODEL
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class Source(text: String = "", references: IdentityMap[Any, Range] = IdentityMap()) {
	implicit class ExtendedIdentityMap(m: IdentityMap[Any, Range]) {
	}

	def ++(text: String): Source = this ++ Source(text)
	def ++(other: Source) = {
		def shifted(m: IdentityMap[Any, Range], n: Int): IdentityMap[Any, Range] = m.map{ case (k, v) => (k, if (v.isInclusive) v.start + n to v.end + n else v.start + n until v.end + n) }

		Source(text + other.text, shifted(other.references, text.size) ++ references)
	}

	def referencing(target: Any) = {
		def fillingCount(s: Iterator[_]) = s.takeWhile(" \t\n".contains(_)).size

		val start = fillingCount(text.iterator)
		val end = text.size - fillingCount(text.reverseIterator)

		copy(references = references + (target, start until end))
	}

	def dropReferences = Source(text)
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// VIEW PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class SourceViewPreferences(constants: Map[Symbol, String], formattingPreferences: FormattingPreferences)
case class FormattingPreferences( //TODO!! Adjust Locations and Orders to work on grammars instead of encoders
		//		protected val spacing: Set[LocationRule[Any]] = Set(),
		protected val tabulationSequence: String = "\t",
		protected val tabulationSize: Int = 1 //		protected val lineBreaks: Map[LocationRule[Any], Int] = Map(),
		//		protected val tabulationLevelIncrements: Map[LocationRule[Any], Int] = Map(),
		//		protected val sortOrders: Set[Order[_]] = Set()
		) {
	//	def tabulationLevelIncrement(locationKey: LocationKey[_]) = tabulationLevelIncrements.collectFirst{ case (l, i) if l.matches(locationKey) => i } getOrElse 0
	//	def space(locationKey: LocationKey[_]) = spacing.collectFirst{ case l if l.matches(locationKey) => " " } getOrElse ""
	//	def lineBreak(locationKey: LocationKey[_]) = "\n" * lineBreaks.collect{ case (l, count) if l.matches(locationKey) => count }.sum
	//	def tabulation(level: Int) = tabulationSequence * tabulationSize * level
	//	def sortOrder[T](target: GrammarEncoder[List[T]]) = sortOrders.collectFirst { case order @ Order(`target`) => order.criteria.asInstanceOf[(T, T) => Boolean] }
}
//case class Order[T](target: GrammarEncoder[List[T]])(val criteria: (T, T) => Boolean)
//trait Location[+T] {
//	def on[U >: T](target: U) = LocationKey(this, target)
//	def apply(condition: PartialFunction[Any, Boolean] = null) = LocationRule(this)(Option(condition))
//}
//case class After[T](target: GrammarEncoder[T]) extends Location[T]
//case class Before[T](target: GrammarEncoder[T]) extends Location[T]
//case class On[T](target: GrammarEncoder[T]) extends Location[T]
//case class InBetween[T](target: GrammarEncoder[List[T]]) extends Location[(T, T, List[T])]
//
//protected case class LocationKey[+T](val location: Location[T], val target: T)
//
//protected case class LocationRule[+T](location: Location[T])(condition: Option[PartialFunction[Any, Boolean]]) {
//	def matches[U >: T](key: LocationKey[U]) = {
//		key.location == location && condition.forall{ condition => condition.applyOrElse(key.target, { _: Any => false })
//		}
//	}
//}