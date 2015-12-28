package org.uqbar.thin.more.views.source

import java.lang.reflect.Method

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls

import scala.util.matching.Regex

trait Grammars extends Parsers {

	def encoder[E](grammar: GrammarDefinition[E, Any])(implicit options: GrammarPreferences): Encoder[E] = grammar match {
		case Empty => new EmptyEncoder()
		case Fail => throw new RuntimeException("grammar reached fail point")
		case v: Value => new ValueEncoder()
		case t: Terminal => new TerminalEncoder(t.key)
		case a: Append[_, _, _, _] => new AppendEncoder(encoder(a.left), encoder(a.right))
		case t: Transform[_, _, _, _] => new TransformEncoder(encoder(t.target))(t.tx)
		case o: Or[_, _, _, _, _, _] => new OrEncoder(encoder(o.left), encoder(o.right))
		case r: Repeat[_, _] => new RepeatEncoder(encoder(r.body), encoder(r.separator))
	}

	def decoder[D](grammar: GrammarDefinition[Nothing, D])(implicit options: GrammarPreferences): CodeParser[D] = grammar match {
		case Empty => EmptyParser
		case Fail => throw new RuntimeException("grammar reached fail point")
		case v: Value => new ValueParser(v.restriction)
		case t: Terminal => new TerminalParser(t.key)
		case a: Append[_, _, _, _] => new AppendParser(decoder(a.left), decoder(a.right))
		case t: Transform[_, _, _, _] => new TransformParser(decoder(t.target))(t.xt)
		case o: Or[_, _, _, _, _, _] => new OrParser(decoder(o.left), decoder(o.right))
		case r: Repeat[_, _] => new RepeatParser(decoder(r.body), decoder(r.separator))
	}

	sealed trait GrammarDefinition[-E, +D] {
		def encoder(implicit options: GrammarPreferences) = Grammars.this.encoder(this)
		def decoder(implicit options: GrammarPreferences) = Grammars.this.decoder(this)
	}

	object Empty extends GrammarDefinition[Any, Null]

	object Fail extends GrammarDefinition[Any, Nothing]

	class Value(val restriction: Regex) extends GrammarDefinition[String, String]

	class Terminal(val key: Symbol) extends GrammarDefinition[String, String]

	class Append[LE, LD, RE, RD](_left: => GrammarDefinition[LE, LD], _right: => GrammarDefinition[RE, RD]) extends GrammarDefinition[(LE, RE), (LD, RD)] {
		lazy val left = _left
		lazy val right = _right
	}

	class Transform[E, D, TE, TD](_target: => GrammarDefinition[E, D])(val tx: TE => E)(val xt: D => TD) extends GrammarDefinition[TE, TD] {
		lazy val target = _target
	}

	class Or[LE <: E, RE <: E, E, LD <: D, RD <: D, D](_left: => GrammarDefinition[LE, LD], _right: => GrammarDefinition[RE, RD]) extends GrammarDefinition[E, D] {
		lazy val left = _left
		lazy val right = _right
	}

	class Repeat[E, D](_body: => GrammarDefinition[E, D], _separator: => GrammarDefinition[Null, Any] = Empty) extends GrammarDefinition[List[E], List[D]] {
		lazy val body = _body
		lazy val separator = _separator
	}

}

trait GrammarSugar extends Grammars {

	type Grammar[T] = GrammarDefinition[T, T]

	implicit def SymbolToGrammar(s: Symbol) = new Terminal(s)
	implicit def RegexToGrammar(r: Regex) = new Value(r)
	implicit def StringToGrammar(s: String) = new Value(s.r)

	implicit class GrammarExt[E, D](g: GrammarDefinition[E, D]) extends Grammarable[E, D](g)
	implicit class SymbolExt(s: Symbol) extends Grammarable(SymbolToGrammar(s))
	implicit class RegexExt(r: Regex) extends Grammarable(RegexToGrammar(r))

	abstract class Grammarable[LE, LD](grammar: GrammarDefinition[LE, LD]) {
		def ~[RE, RD](other: => GrammarDefinition[RE, RD]) = new Append(grammar, other)
		def |[E >: LE, RE <: E, D >: LD, RD <: D](other: => GrammarDefinition[RE, RD]) = new Or[LE, RE, E, LD, RD, D](grammar, other)
		def * = new Repeat(grammar)
		def *~(separator: GrammarDefinition[Any, Null]) = new Repeat(grammar, separator)
		def ~>[RE, RD](other: => GrammarDefinition[RE, RD]) = grammar ~ other ^^ { x: RE => (null.asInstanceOf[LE], x) } -> { t: (LD, RD) => t._2 }
		def <~[RE, RD](other: => GrammarDefinition[RE, RD]) = grammar ~ other ^^ { x: LE => (x, null.asInstanceOf[RE]) } -> { t: (LD, RD) => t._1 }
		def ^^[TE, TD](txs: (TE => LE, LD => TD)) = new Transform(grammar)(txs._1)(txs._2)
	}

	//	implicit def companionToTransformation1[P1, O <: AnyRef](c: Product{def apply(p: P1): O}): (O => P1, P1 => O) = (
	//		{o: O => c.getClass.getMethods.find{m: Method => m.getName == "unapply"}.get.invoke(c,o).asInstanceOf[Option[P1]].get },
	//		{ case p1 => c.apply(p1) }
	//	)

	implicit def companionToTransformationN[T <: Product, NT <: Tuple2[_, _], O <: AnyRef](c: { def tupled: T => O }): (O => NT, NT => O) = (
		{ o: O => c.getClass.getMethods.find{ m: Method => m.getName == "unapply" }.get.invoke(c, o).asInstanceOf[Option[T]].get.productIterator.toList.reverse.reduceLeft{ (a, e) => (e, a) }.asInstanceOf[NT] },
		{ nt: NT =>
			def flattened(t: Tuple2[_, _]): List[Any] = t match {
				case (a, b: Tuple2[_, _]) => a :: flattened(b)
				case (a, b) => a :: b :: Nil
			}
			c.tupled((flattened(nt) match {
				case p1 :: p2 :: Nil => (p1, p2)
				case p1 :: p2 :: p3 :: Nil => (p1, p2, p3)
				case p1 :: p2 :: p3 :: p4 :: Nil => (p1, p2, p3, p4)
				case p1 :: p2 :: p3 :: p4 :: p5 :: Nil => (p1, p2, p3, p4, p5)
				case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: Nil => (p1, p2, p3, p4, p5, p6)
				case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: Nil => (p1, p2, p3, p4, p5, p6, p7)
				case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: Nil => (p1, p2, p3, p4, p5, p6, p7, p8)
				case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: Nil => (p1, p2, p3, p4, p5, p6, p7, p8, p9)
				case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: Nil => (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
				case p1 :: p2 :: p3 :: p4 :: p5 :: p6 :: p7 :: p8 :: p9 :: p10 :: p11 :: Nil => (p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
				//TODO? etc
			}).asInstanceOf[T])
		}
	)
}

//▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀▀
// PREFERENCES
//▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄▄

case class GrammarPreferences(
	terminals: Map[Symbol, String],
	encodingPreferences: EncodingPreferences)

case class EncodingPreferences(
		protected val spacing: Set[LocationRule[Any]] = Set(),
		protected val tabulationSequence: String = "\t",
		protected val tabulationSize: Int = 1,
		protected val lineBreaks: Map[LocationRule[Any], Int] = Map(),
		protected val tabulationLevelIncrements: Map[LocationRule[Any], Int] = Map(),
		protected val sortOrders: Set[Order[_]] = Set()) {
	def tabulationLevelIncrement(locationKey: LocationKey[_]) = tabulationLevelIncrements.collectFirst{ case (l, i) if l.matches(locationKey) => i } getOrElse 0
	def space(locationKey: LocationKey[_]) = spacing.collectFirst{ case l if l.matches(locationKey) => " " } getOrElse ""
	def lineBreak(locationKey: LocationKey[_]) = "\n" * lineBreaks.collect{ case (l, count) if l.matches(locationKey) => count }.sum
	def tabulation(level: Int) = tabulationSequence * tabulationSize * level
	def sortOrder[T](target: Encoder[List[T]]) = sortOrders.collectFirst { case order @ Order(`target`) => order.criteria.asInstanceOf[(T, T) => Boolean] }
}

case class Order[T](target: Encoder[List[T]])(val criteria: (T, T) => Boolean)

trait Location[+T] {
	def on[U >: T](target: U) = LocationKey(this, target)
	def apply(condition: PartialFunction[Any, Boolean] = null) = LocationRule(this)(Option(condition))
}
case class After[T](target: Encoder[T]) extends Location[T]
case class Before[T](target: Encoder[T]) extends Location[T]
case class On[T](target: Encoder[T]) extends Location[T]
case class InBetween[T](target: Encoder[List[T]]) extends Location[(T, T, List[T])]

protected case class LocationKey[+T](val location: Location[T], val target: T)

protected case class LocationRule[+T](location: Location[T])(condition: Option[PartialFunction[Any, Boolean]]) {
	def matches[U >: T](key: LocationKey[U]) = {
		key.location == location && condition.forall{ condition => condition.applyOrElse(key.target, { _: Any => false })
		}
	}
}