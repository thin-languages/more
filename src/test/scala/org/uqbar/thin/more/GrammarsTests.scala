package org.uqbar.thin.more

import org.scalatest.FreeSpec
import org.uqbar.thin.more.views.source.Grammars
import org.scalatest.Matchers
import org.uqbar.thin.more.views.source._

class GrammarsTest extends FreeSpec with GrammarTest with GrammarSugar {

	"Grammar" - {

		implicit val preferences: GrammarPreferences = GrammarPreferences(Map(), EncodingPreferences())
		
		"encoder" - {
			"should be obtainable from" - {
				"Empty" in {
					Empty.encoder shouldBe an[EmptyEncoder]
				}
				"Lexeme" in {
					new Lexeme("foo".r).encoder shouldBe a[LexemeEncoder]
				}
				"Constant" in {
					new Constant('foo).encoder shouldBe a[ConstantEncoder]
				}
				"Transform" in {
					new Transform(new Constant('foo))({n: Int => "Foo"})({s: String => 7}).encoder shouldBe a[TransformEncoder[_,_]]
				}
				"Append" in {
					new Append(new Constant('foo), new Constant('bar)).encoder shouldBe an[AppendEncoder[_,_]]
				}
				"Or" in {
					new Or[String,String,String,String,String,String](new Constant('foo), new Constant('bar)).encoder shouldBe an[OrEncoder[_,_,_]]
				}
				"Repeat" in {
					new Repeat(new Constant('foo)).encoder shouldBe a[RepeatEncoder[_]]
				}
			}
			
			"should not be obtainable from" - {
				"Fail" in {
					a [GrammarException] should be thrownBy Fail.encoder
				}
			}
		}
		
		"parser" - {
			"should be obtainable from" - {
				"Empty" in {
					Empty.decoder should be(EmptyParser)
				}
				"Lexeme" in {
					new Lexeme("foo".r).decoder shouldBe a[LexemeParser]
				}
				"Constant" in {
					new Constant('foo).decoder shouldBe a[ConstantParser]
				}
				"Transform" in {
					new Transform(new Constant('foo))({n: Int => "Foo"})({s: String => 7}).decoder shouldBe a[TransformParser[_,_]]
				}
				"Append" in {
					new Append(new Constant('foo), new Constant('bar)).decoder shouldBe an[AppendParser[_,_]]
				}
				"Or" in {
					new Or[String,String,String,String,String,String](new Constant('foo), new Constant('bar)).decoder shouldBe an[OrParser[_,_,_]]
				}
				"Repeat" in {
					new Repeat(new Constant('foo)).decoder shouldBe a[RepeatParser[_]]
				}
			}
			
			"should not be obtainable from" - {
				"Fail" in {
					a [GrammarException] should be thrownBy Fail.decoder
				}
			}
		}
		
		"sugar" - {
			"constants should be obtainable from symbols" in {
				fail() //TODO
			}

			"lexemes should be obtainable from regular expressions" in {
				fail() //TODO
			}
			
			"lexemes should be obtainable from strings" in {
				fail() //TODO
			}

			"append should be obtainable from binary operator" in {
				fail() //TODO
			}
			
			"append ignoring left should be obtainable from binary operator" in {
				fail() //TODO
			}
			
			"append ignoring right should be obtainable from binary operator" in {
				fail() //TODO
			}
			
			"or should be obtainable from binary operator" in {
				fail() //TODO
			}
			
			"repeat without separator should be obtainable from binary operator" in {
				fail() //TODO
			}

			"repeat with separator should be obtainable from binary operator" in {
				fail() //TODO
			}
			
			"transform should be obtainable from binary operator" in {
				fail() //TODO
			}

			"companion objects of case classes with arity 1 should be usable as transform argument" in {
				fail() //TODO
			}
			
			"companion objects of case classes with arity 2 should be usable as transform argument" in {
				fail() //TODO
			}
			
			"companion objects of case classes with arity +3 should be usable as transform argument" in {
				fail() //TODO
			}
			
		}
	}
}

trait GrammarTest extends Matchers with Grammars {
//	case class beGrammar[G <: Grammarable[E,D]](target: G) extends Matcher[G] {
//	}
}