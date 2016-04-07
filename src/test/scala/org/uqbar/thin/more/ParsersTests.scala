package org.uqbar.thin.more

import org.scalatest.Finders
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.uqbar.thin.more.views.source.Parsers

class JavalessParserTest extends FreeSpec with Matchers with Parsers {

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
        "Var" shouldNot beParsedTo("var2")
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
