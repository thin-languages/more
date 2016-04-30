package org.uqbar.thin.more.views.source

import org.scalatest.FreeSpec
import org.scalatest.Matchers

class GrammarsTest extends FreeSpec with Matchers with GrammarSugar {

	"Grammar" - {

		"sugar" - {

			val foo = new Constant('foo)
			val bar = new Lexeme("bar".r)

			"constants should be obtainable from symbols" in {
				val grammar: Grammar[_] = 'foo
				grammar should be(foo)
			}

			"lexemes should be obtainable from regular expressions" in {
				val grammar: Grammar[_] = "bar".r
				grammar should be(bar)
			}

			"lexemes should be obtainable from strings" in {
				val grammar: Grammar[_] = "bar"
				grammar should be(bar)
			}

			"append should be obtainable from binary operator" in {
				foo ~ bar should be(new Append(foo, bar))
			}

			"append ignoring left should be obtainable from binary operator" in {
				val grammar = foo ~> bar

				grammar should be(new Transform(foo ~ bar)(null: String => (String, String))(null: ((String, String)) => String))
				grammar.tx("bar") should be(null, "bar")
				grammar.xt("foo", "bar") should be("bar")
			}

			"append ignoring right should be obtainable from binary operator" in {
				val grammar = foo <~ bar

				grammar should be(new Transform(foo ~ bar)(null: String => (String, String))(null: ((String, String)) => String))
				grammar.tx("foo") should be("foo", null)
				grammar.xt("foo", "bar") should be("foo")
			}

			"or should be obtainable from binary operator" in {
				foo | bar should be(new Or[String, String, String, String, String, String](foo, bar))
			}

			"repeat without separator should be obtainable from binary operator" in {
				foo.* should be(new Repeat(foo))
			}

			"repeat with separator should be obtainable from binary operator" in {
				foo *~ bar should be(new Repeat(foo, bar))
			}

			"transform should be obtainable from binary operator" in {
				foo ^^ (null: Int => String, null: String => Int) should be(new Transform(foo)(null: Int => String)(null: String => Int))
			}

			"companion objects of case classes with arity 1 should be usable as transform argument" in {
				case class C(s: String)

				val grammar = foo ^^ C

				grammar should be(new Transform(foo)(null: C => String)(null: String => C))
				grammar.tx(C("foo")) should be("foo")
				grammar.xt("foo") should be(C("foo"))
			}

			"companion objects of case classes with arity 2 should be usable as transform argument for sequences" in {
				case class C(s: String, t: String)

				val grammar = foo ~ foo ^^ C

				grammar should be(new Transform(foo ~ foo)(null: C => (String, String))(null: ((String, String)) => C))
				grammar.tx(C("foo", "bar")) should be("foo", "bar")
				grammar.xt("foo", "bar") should be(C("foo", "bar"))
			}
			"companion objects of case classes with arity +3 should be usable as transform argument for sequences" in {
				case class C(s: String, t: String, u: String, v: String)

				val grammar = foo ~ foo ~ foo ~ foo ^^ C

				grammar should be(new Transform(foo ~ foo ~ foo ~ foo)(null: C => (((String, String), String), String))(null: ((((String, String), String), String)) => C))
				grammar.tx(C("foo", "bar", "baz", "meh")) should be((("foo", "bar"), "baz"), "meh")
				grammar.xt((("foo", "bar"), "baz"), "meh") should be(C("foo", "bar", "baz", "meh"))
			}

		}
	}
}