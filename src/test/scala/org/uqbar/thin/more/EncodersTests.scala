package org.uqbar.thin.more

import org.scalatest.FreeSpec
import org.scalatest.Matchers


class EncodersTest extends FreeSpec with Matchers {
	
}

//trait T
//case class X(s: String) extends T
//case class Y(s: String, n: Int) extends T
//
//class EncodersTest extends FreeSpec with Encoders with EncoderMatchers {
//
//	implicit val terminals = Map[Symbol, String](
//		'Foo -> "Foo",
//		'Bar -> "Bar",
//		'Meh -> "Meh",
//		'X -> "X:",
//		'Y -> "Y:",
//		': -> ":"
//	)
//
//	implicit val preferences = new EncoderPreferences()
//
//	"Encoder" - {
//
//		"Reference" - {
//			"output text should be the encoded object toString" in {
//				& encode (1) should resultIn("1")(1 -> 0.to(0))
//				& encode ("Foo") should resultIn("Foo")("Foo" -> 0.to(2))
//
//				val option = Some('c')
//				& encode (option) should resultIn("Some(c)")(option -> 0.to(6))
//			}
//		}
//
//		"Constant" - {
//			"output text should be the given constant" in {
//				Constant('Foo) encode (()) should resultIn("Foo")(() -> 0.to(2))
//			}
//
//			"should be implicitly created from a Symbol" in {
//				('Foo: Encoder[_]) should be (Constant('Foo))
//			}
//		}
//
//		"Append" - {
//			
//			"output text should be the result of appending the output of the given encoders" in {
//				'Foo ~ 'Bar encode (()) should resultIn("FooBar")(() -> 0.to(5))
//				'Foo ~ 'Bar ~ 'Meh encode (()) should resultIn("FooBarMeh")(() -> 0.to(8))
//				'Foo ~ & ~ 'Bar encode (5) should resultIn("Foo5Bar")(5 -> 0.to(6))
//			}
//
//			"should have syntactic sugar to be crated from target encoders" in {
//				'Foo ~ & ~ 'Bar should be (Append(Append(Constant('Foo), &), Constant('Bar)))
//			}
//		}
//
//		"Transform" - {
//			"given a transform function, the output should be the output of the given encoder, applied to the target transformed by that function" in {
//				val target = Some(58)
//
//				&{ (_: Option[Any]).get } encode (target) should resultIn("58")(target -> 0.to(1), 58 -> 0.to(1))
//				('X ~ &){ (_: Option[Any]).get } encode (target) should resultIn("X:58")(target -> 0.to(3), 58 -> 0.to(3))
//			}
//
//			"should have syntactic sugar to be crated from target encoders" in {
//				&{ (_: Any).toString } should be (Transform(&){ x: Any => x.toString })
//			}
//		}
//
//		"Or" - {
//			"output should be the output of the left encoder or, if it fails, the output of the right one" in {
//				val x = 'X ~ &{ (_: X).s }
//				val y = 'Y ~ &{ (_: Y).s } ~ ': ~ &{ (_: Y).n }
//
//				val targetX = X("foo")
//				val targetY = Y("bar", 5)
//
//				(x | y: Encoder[T]) encode (targetX) should resultIn("X:foo")(targetX -> 0.to(4), "foo" -> 2.until(5))
//				y | x encode (targetX) should resultIn("X:foo")(targetX -> 0.to(4), "foo" -> 2.until(5))
//				x | y encode (targetY) should resultIn("Y:bar:5")(targetY -> 0.to(6), "bar" -> 2.until(5), 5 -> 6.until(7))
//				(y | x: Encoder[T]) encode (targetY) should resultIn("Y:bar:5")(targetY -> 0.to(6), "bar" -> 2.until(5), 5 -> 6.until(7))
//			}
//
//			"should have syntactic sugar to be crated from target encoders" in {
//				'Foo | 'Bar | 'Meh should be (Or(Or(Constant('Foo), Constant('Bar)), Constant('Meh)))
//			}
//		}
//
//		"RepSep" - {
//			val encoder = &.*~(':)
//
//			"if target list is empty, output should be the empty" in {
//				encoder encode (Nil) should resultIn("")(Nil -> 0.until(0))
//			}
//
//			"if target list has only one element, output should be the output of target encoder, applied to that element, with no separator" in {
//				val target = List(1)
//				encoder encode (target) should resultIn("1")(target -> 0.to(0), 1 -> 0.to(0))
//			}
//
//			"if target list has more than one element, output should be the output of target encoder, applied to each element, separated by the output of the separator encoder" in {
//				val target = List(1, 2, 3)
//				encoder encode (target) should resultIn("1:2:3")(target -> 0.to(4), 1 -> 0.to(0), 2 -> 2.to(2), 3 -> 4.to(4))
//			}
//
//			"should have syntactic sugar to be crated from target encoders" in {
//				encoder should be (RepSep(&, Constant(':)))
//			}
//
//			"should have syntactic sugar for no separator repetition, to be crated from target encoders" in {
//				&.* should be (RepSep(&, Empty))
//			}
//		}
//	}
//	
//	"Preferences" - {
//		"sortOrders should sort" in {
//			implicit val preferences = EncoderPreferences(sortOrders = Set(Order[Char](&.*){ (a,b) => a.isLetter && b.isDigit }))
//			val target = List('1','a','3','b','5')
//			
//			&.* encode (target) should resultIn("ab135")(target-> 0.to(4),'1'->2.to(2),'a'->0.to(0),'3'->3.to(3),'b'->1.to(1),'5'->4.to(4))
//		}
//	}
//}