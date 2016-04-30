package org.uqbar.thin.more.views.source

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher
import org.uqbar.thin.more.views.source.SourceDecoders.SourceDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.EmptyDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.LexemeDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.ConstantDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.AppendDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.TransformDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.OrDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.RepeatDecoder

class SourceDecodersTests extends FreeSpec with Matchers {

  val terminals = Map[Symbol, String](
    'Foo -> "Foo",
    'X -> "X",
    ': -> ":")

  implicit val preferences = SourceViewPreferences(terminals, new FormattingPreferences)

  val lowerCaseLexemeDecoder = new LexemeDecoder("[a-z]+".r)
  val fooConstantDecoder = new ConstantDecoder('Foo)
  
  "SourceDecoder" - {

    "EmptyDecoder" - {
      implicit val decoder = EmptyDecoder

      "should success for empty string" in {
        "" should beParsedTo(null)
      }

      "should fail for non empty string" in {
        "somethign" shouldNot beParsed()
      }
    }

    "LexemeDecoder" - {
      implicit val decoder = lowerCaseLexemeDecoder

      "should success when string matchs the regex" in {
        "var" should beParsedTo("var")
      }

      "should fail when string doesn't match the regex" in {
        "Var" shouldNot beParsed()
      }
    }

    "ConstantDecoder" - {
      implicit val decoder = fooConstantDecoder

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

    "AppendDecoder" - {
      implicit val decoder = new AppendDecoder(new ConstantDecoder('X), new ConstantDecoder(':))

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

    "TransformDecoder" - {
      implicit val decoder = new TransformDecoder(lowerCaseLexemeDecoder)( x => x.length())

      "should return transformed value" in {
        "value" should beParsedTo(5)
      }

      "should fail when parser fails" in {
        "Foo" shouldNot beParsed()
      }
    }

    "OrDecoder" - {
      implicit val decoder = new OrDecoder(fooConstantDecoder, lowerCaseLexemeDecoder)

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

    "RepeatDecoder" - {
      implicit val decoder = new RepeatDecoder(lowerCaseLexemeDecoder, new ConstantDecoder(':))

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

case class beParsed(implicit decoder: SourceDecoder[_]) extends Matcher[String] {
  def apply(target: String) = {
    val result = decoder.apply(target)

    MatchResult(
      result.isSuccess,
      "Parse failed! $result",
      "Parse didn't fail! $result")
  }
}

case class beParsedTo[T](expected: T)(implicit decoder: SourceDecoder[T]) extends Matcher[String] {
  def apply(target: String) = {
    val result = decoder.apply(target)

    MatchResult(
      result.map { decoded => decoded == expected }.getOrElse(false),
      result.map { decoded => s"Parsed $decoded did not equal $expected" }.getOrElse(s"Parse failed! $result"),
      result.map { decoded => s"Parsed $decoded was equal to $expected" }.getOrElse(s"Parse didn't fail! $result"))
  }
}  
