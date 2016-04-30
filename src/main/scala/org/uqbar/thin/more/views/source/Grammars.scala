package org.uqbar.thin.more.views.source

import java.lang.reflect.Method

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.util.matching.Regex

import org.uqbar.thin.more.views.source.SourceDecoders.AppendDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.ConstantDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.EmptyDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.LexemeDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.OrDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.RepeatDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.SourceDecoder
import org.uqbar.thin.more.views.source.SourceDecoders.TransformDecoder
import org.uqbar.thin.more.views.source.SourceEncoders.AppendEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.ConstantEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.EmptyEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.LexemeEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.OrEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.RepeatEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.SourceEncoder
import org.uqbar.thin.more.views.source.SourceEncoders.TransformEncoder

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// GRAMMARS
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class GrammarException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)

object GrammarDefinition {
	implicit def GrammarToDecoder[D](grammar: GrammarDefinition[Nothing, D])(implicit preferences: SourceViewPreferences): SourceDecoder[D] = grammar match {
		case Empty => EmptyDecoder
		case Fail => throw new GrammarException("grammar reached fail point")
		case l: Lexeme => new LexemeDecoder(l.restriction)
		case c: Constant => new ConstantDecoder(c.key)
		case a: Append[_, _, _, _] => new AppendDecoder(a.left, a.right)
		case t: Transform[_, _, _, _] => new TransformDecoder(t.target)(t.xt)
		case o: Or[_, _, _, _, _, _] => new OrDecoder(o.left, o.right)
		case r: Repeat[_, _] => new RepeatDecoder(r.body, r.separator)
	}

	implicit def GrammarToEncoder[E](grammar: GrammarDefinition[E, Any])(implicit preferences: SourceViewPreferences): SourceEncoder[E] = grammar match {
		case Empty => new EmptyEncoder
		case Fail => throw new GrammarException("grammar reached fail point")
		case l: Lexeme => new LexemeEncoder
		case c: Constant => new ConstantEncoder(c.key)
		case a: Append[_, _, _, _] => new AppendEncoder(a.left, a.right)
		case t: Transform[_, _, _, _] => new TransformEncoder(t.target)(t.tx)
		case o: Or[_, _, _, _, _, _] => new OrEncoder(o.left, o.right)
		case r: Repeat[_, _] => new RepeatEncoder(r.body, r.separator)
	}
}
sealed trait GrammarDefinition[-E, +D]

object Empty extends GrammarDefinition[Any, Null]

object Fail extends GrammarDefinition[Any, Nothing]

class Lexeme(val restriction: Regex) extends GrammarDefinition[String, String] {
	override def equals(obj: Any) = obj match {
		case g: Lexeme => g.restriction.toString == restriction.toString
		case _ => super.equals(obj)
	}
	override def hashCode = restriction.hashCode
}

class Constant(val key: Symbol) extends GrammarDefinition[String, String] {
	override def equals(obj: Any) = obj match {
		case g: Constant => g.key == key
		case _ => super.equals(obj)
	}
	override def hashCode = key.hashCode
}

class Transform[E, D, TE, TD](_target: => GrammarDefinition[E, D])(val tx: TE => E)(val xt: D => TD) extends GrammarDefinition[TE, TD] {
	lazy val target = _target

	override def equals(obj: Any) = obj match {
		case g: Transform[E, D, TE, TD] => g.target == target
		case _ => super.equals(obj)
	}
	override def hashCode = target.hashCode
}

class Append[LE, LD, RE, RD](_left: => GrammarDefinition[LE, LD], _right: => GrammarDefinition[RE, RD]) extends GrammarDefinition[(LE, RE), (LD, RD)] {
	lazy val left = _left
	lazy val right = _right

	override def equals(obj: Any) = obj match {
		case g: Append[LE, LD, RE, RD] => g.left == left && g.right == right
		case _ => super.equals(obj)
	}
	override def hashCode = left.hashCode + right.hashCode
}

class Or[LE <: E, RE <: E, E, LD <: D, RD <: D, D](_left: => GrammarDefinition[LE, LD], _right: => GrammarDefinition[RE, RD]) extends GrammarDefinition[E, D] {
	lazy val left = _left
	lazy val right = _right

	override def equals(obj: Any) = obj match {
		case g: Or[LE, RE, E, LE, RD, D] => g.left == left && g.right == right
		case _ => super.equals(obj)
	}
	override def hashCode = left.hashCode + right.hashCode
}

class Repeat[E, D](_body: => GrammarDefinition[E, D], _separator: => GrammarDefinition[Null, Any] = Empty) extends GrammarDefinition[List[E], List[D]] {
	lazy val body = _body
	lazy val separator = _separator

	override def equals(obj: Any) = obj match {
		case g: Repeat[E, D] => g.body == body && g.separator == separator
		case _ => super.equals(obj)
	}
	override def hashCode = body.hashCode + separator.hashCode
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// SYNTACTIC SUGAR
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

trait GrammarSugar {

	type Grammar[T] = GrammarDefinition[T, T]

	implicit def SymbolToGrammar(s: Symbol) = new Constant(s)
	implicit def RegexToGrammar(r: Regex) = new Lexeme(r)
	implicit def StringToGrammar(s: String) = new Lexeme(s.r)

	implicit class GrammarExt[E, D](g: GrammarDefinition[E, D]) extends Grammarable[E, D](g)
	implicit class SymbolExt(s: Symbol) extends Grammarable(SymbolToGrammar(s))
	implicit class RegexExt(r: Regex) extends Grammarable(RegexToGrammar(r))

	abstract class Grammarable[LE, LD](grammar: GrammarDefinition[LE, LD]) {
		def ~[RE, RD](other: => GrammarDefinition[RE, RD]) = new Append(grammar, other)
		def |[E >: LE, RE <: E, D >: LD, RD <: D](other: => GrammarDefinition[RE, RD]) = new Or[LE, RE, E, LD, RD, D](grammar, other)
		def * = new Repeat(grammar)
		def *~(separator: GrammarDefinition[Null, Any]) = new Repeat(grammar, separator)
		def ~>[RE, RD](other: => GrammarDefinition[RE, RD]) = grammar ~ other ^^ { x: RE => (null.asInstanceOf[LE], x) } -> { t: (LD, RD) => t._2 }
		def <~[RE, RD](other: => GrammarDefinition[RE, RD]) = grammar ~ other ^^ { x: LE => (x, null.asInstanceOf[RE]) } -> { t: (LD, RD) => t._1 }
		def ^^[TE, TD](txs: (TE => LE, LD => TD)) = new Transform(grammar)(txs._1)(txs._2)
	}

	implicit def companionToTransformation1[T, O <: AnyRef](companion: T => O): (O => T, T => O) = (
		{ obj: O =>
			val Some(unapply) = companion.getClass.getMethods.find{ m: Method => m.getName == "unapply" }
			val Some(unapplied) = unapply.invoke(companion, obj).asInstanceOf[Option[T]]
			unapplied
		},
		companion.apply
	)

	implicit def companionToTransformationN[T <: Product, NT <: Tuple2[_, _], O <: AnyRef](companion: { def tupled: T => O }): (O => NT, NT => O) = {
		def nestedTupleToList(t: Tuple2[_, _]): List[Any] = t match {
			case (a: Tuple2[_, _], b) => nestedTupleToList(a) ::: b :: Nil
			case (a, b) => a :: b :: Nil
		}

		def objectToNestedTuple(obj: O) = {
			val Some(unapply) = companion.getClass.getMethods.find{ m: Method => m.getName == "unapply" }
			val Some(unapplied) = unapply.invoke(companion, obj).asInstanceOf[Option[T]]
			unapplied.productIterator.reduceLeft{ (e, a) => (e, a) }.asInstanceOf[NT]
		}

		def nestedTupleToFlattenedTuple(nestedTuple: NT) = {
			val elements = nestedTupleToList(nestedTuple).map(_.asInstanceOf[Object])
			val tupleClass = Class.forName("scala.Tuple" + elements.size)
			tupleClass.getConstructors.apply(0).newInstance(elements: _*).asInstanceOf[T]
		}

		(objectToNestedTuple, { nestedTuple: NT => companion.tupled(nestedTupleToFlattenedTuple(nestedTuple)) })
	}
}
