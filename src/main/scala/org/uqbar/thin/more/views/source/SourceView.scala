package org.uqbar.thin.more.views.source

import org.uqbar.thin.more._

trait Sourceable extends Grammars with GrammarSugar {
	def root: GrammarDefinition[LanguageConcept, LanguageConcept] = Fail

	def grammar(target: LanguageConcept): GrammarDefinition[LanguageConcept, _] = root
	def grammar(target: EncoderResult): GrammarDefinition[_, LanguageConcept] = root
}

object SourceView extends LanguageView[EncoderResult, GrammarPreferences, Sourceable] { //TODO: Rename EncoderResult and GrammarPref.
	def encode(target: LanguageConcept)(implicit language: Sourceable, options: GrammarPreferences) = language.grammar(target).encoder.apply(target)
	def decode(target: EncoderResult)(implicit language: Sourceable, options: GrammarPreferences) = language.grammar(target).decoder.apply(target.text)
}