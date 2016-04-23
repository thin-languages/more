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

  val lowerCaseValueParser = new ValueParser("[a-z]+".r)
  val fooTerminalParser = new TerminalParser('Foo)
  
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
      implicit val parser = lowerCaseValueParser

      "should success when string matchs the regex" in {
        "var" should beParsedTo("var")
      }

      "should fail when string doesn't match the regex" in {
        "Var" shouldNot beParsedTo("")
      }
    }

    "TerminalParser" - {
      implicit val parser = fooTerminalParser

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

      "should fail when only left parser success" in {
        "X" shouldNot beParsedTo("","")
      }

      "should fail when only right parser success" in {
        ":" shouldNot beParsedTo("","")
      }

      "should respect the order" in {
        ":X" shouldNot beParsedTo("","")
      }
    }
    

    "TransformParser" - {
      implicit val parser = new TransformParser(lowerCaseValueParser)( x => x.length())

      "should return transformed value" in {
        "value" should beParsedTo(5)
      }

      "should fail when parser fails" in {
        "Foo" shouldNot beParsedTo(0)
      }
    }
    

    "OrParser" - {
      implicit val parser = new OrParser(fooTerminalParser, lowerCaseValueParser)

      "should success when left parser success" in {
        "Foo" should beParsedTo("Foo")
      }
      
      "should success when rigth parser success" in {
        "foo" should beParsedTo("foo")
      }
      
      "should fail when both parsers fail" in {
        "X" shouldNot beParsedTo("")
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
