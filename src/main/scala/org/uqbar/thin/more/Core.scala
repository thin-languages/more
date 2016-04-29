package org.uqbar.thin.more

import scala.util.Try

trait Language

trait LanguageConcept

trait LanguageView[ViewModel, ViewOptions, LanguageRequirement] {
	def encode(target: LanguageConcept)(implicit language: LanguageRequirement, options: ViewOptions): Try[ViewModel]
	def decode(target: ViewModel)(implicit language: LanguageRequirement,  options: ViewOptions): Try[LanguageConcept]
}

class DecodeException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
class EncodeException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)