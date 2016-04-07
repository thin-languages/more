package org.uqbar.thin.more

import org.scalatest.Finders
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.uqbar.thin.more.views.source.Parsers
import org.uqbar.thin.more.views.source.GrammarPreferences
import org.uqbar.thin.more.views.source.EncodingPreferences

class JavalessParserTest extends FreeSpec with Matchers with Parsers {

  val terminals = Map[Symbol, String](
    'Foo -> "Foo",
    'X -> "X",
    ': -> ":")

  implicit val options = GrammarPreferences(terminals, new EncodingPreferences)

  "Parser" - {

    "EmptyParser" - {
      implicit val parser = EmptyParser

      "should success for empty string" in {
        "" should beParsedTo(null)
      }

      "should fail for non empty string" in {
        "somethign" shouldNot beParsedTo(null)
      }
    }

    "ValueParser" - {
      implicit val parser = new ValueParser("[a-z]+".r)

      "should success when string matchs the regex" in {
        "var" should beParsedTo("var")
      }

      "should fail when string doesn't match the regex" in {
        "Var" shouldNot beParsedTo("")
      }
    }

    "TerminalParser" - {
      implicit val parser = new TerminalParser('Foo)

      "should success when string matchs with terminal" in {
        "Foo" should beParsedTo("Foo")
      }

      "should fail when string doesn't match with terminal" in {
        "foo" shouldNot beParsedTo("")
      }

      "should fail when string matchs other terminal" in {
        "X" shouldNot beParsedTo("")
      }
    }

    "AppendParser" - {
      implicit val parser = new AppendParser(new TerminalParser('X), new TerminalParser(':))

      "should success when both parsers success" in {
        "X:" should beParsedTo("X", ":")
      }

      "should fail when only first parser success" in {
        "X" shouldNot beParsedTo("","")
      }

      "should fail when only second parser success" in {
        ":" shouldNot beParsedTo("","")
      }

      "should respect the order" in {
        ":X" shouldNot beParsedTo("","")
      }
    }
  }
}

case class beParsedTo[T](expected: T)(implicit parser: Parsers#CodeParser[T]) extends Matcher[String] {
  def apply(target: String) = {
    val result = parser.apply(target)

    MatchResult(
      result.map { parsed => parsed == expected }.getOrElse(false),
      result.map { parsed => s"Parsed $parsed did not equal $expected" }.getOrElse(s"Parse failed! $result"),
      result.map { parsed => s"Parsed $parsed was equal to $expected" }.getOrElse(s"Parse didn't fail! $result"))
  }
}  
