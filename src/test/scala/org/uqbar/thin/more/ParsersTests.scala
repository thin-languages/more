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
        "somethign" shouldNot beParsed()
      }
    }

    "ValueParser" - {
      implicit val parser = lowerCaseValueParser

      "should success when string matchs the regex" in {
        "var" should beParsedTo("var")
      }

      "should fail when string doesn't match the regex" in {
        "Var" shouldNot beParsed()
      }
    }

    "TerminalParser" - {
      implicit val parser = fooTerminalParser

      "should success when string matchs with terminal" in {
        "Foo" should beParsedTo("Foo")
      }

      "should fail when string doesn't match with terminal" in {
        "foo" shouldNot beParsed()
      }

      "should fail when string matchs other terminal" in {
        "X" shouldNot beParsed()
      }
    }

    "AppendParser" - {
      implicit val parser = new AppendParser(new TerminalParser('X), new TerminalParser(':))

      "should success when both parsers success" in {
        "X:" should beParsedTo("X", ":")
      }

      "should fail when only left parser success" in {
        "X" shouldNot beParsed()
      }

      "should fail when only right parser success" in {
        ":" shouldNot beParsed()
      }

      "should respect the order" in {
        ":X" shouldNot beParsed()
      }
    }

    "TransformParser" - {
      implicit val parser = new TransformParser(lowerCaseValueParser)( x => x.length())

      "should return transformed value" in {
        "value" should beParsedTo(5)
      }

      "should fail when parser fails" in {
        "Foo" shouldNot beParsed()
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
        "X" shouldNot beParsed()
      }
    }

    "RepeatParser" - {
      implicit val parser = new RepeatParser(lowerCaseValueParser, new TerminalParser(':))

      "should success for empty string" in {
        "" should beParsedTo(List.empty[String])
      }
      
      "should success for single item" in {
        "one" should beParsedTo(List("one"))
      }
      
      "should success for multiples items" in {
        "one:two:three" should beParsedTo(List("one","two","three"))
      }
      
      "should fail when item parser fails" in {
        "one:Two" shouldNot beParsed()
      }
      
      "should fail when separator parser fails" in {
        "one|two" shouldNot beParsed()
      }
      
      "should fail when item is missing" in {
        "one:" shouldNot beParsed()
        ":two" shouldNot beParsed()
        "one::three" shouldNot beParsed()
      }
    }
  }
}

case class beParsed(implicit parser: Parsers#CodeParser[_]) extends Matcher[String] {
  def apply(target: String) = {
    val result = parser.apply(target)

    MatchResult(
      result.isSuccess,
      "Parse failed! $result",
      "Parse didn't fail! $result")
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
