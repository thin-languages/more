package org.uqbar.thin.more.views.source

import scala.language.existentials
import scala.language.implicitConversions
import scala.util.Try

object SourceEncoders {

	abstract class SourceEncoder[-T](implicit preferences: SourceViewPreferences) {
		implicit def StringToSource(s: String) = Source(s)

		def apply(target: T, level: Int = 0) = for {
			content <- encode(target, level /*TODO!! + preferences.tabulationLevelIncrement(On(this) on target) */ )
		} yield formated(content referencing target, target)

		protected def formated(result: Source, target: T) =
			//TODO!!preferences.lineBreak(After(this) on target) ++
			//TODO!!preferences.space(Before(this) on target) ++
			result //++
		//TODO!!preferences.lineBreak(After(this) on target) ++
		//TODO!!preferences.space(After(this) on target)

		protected def tabulate(target: Source, level: Int): Source = /*TODO!!preferences.tabulation(level) ++*/ target

		protected def encode(target: T, level: Int): Try[Source]
	}

	class EmptyEncoder()(implicit preferences: SourceViewPreferences) extends SourceEncoder[Any] {
		protected def encode(target: Any, level: Int) = Try("")
	}

	class LexemeEncoder()(implicit preferences: SourceViewPreferences) extends SourceEncoder[Any] {
		protected def encode(target: Any, level: Int) = Try(tabulate(target.toString, level))
	}

	class ConstantEncoder(terminal: Symbol)(implicit preferences: SourceViewPreferences) extends SourceEncoder[Any] {
		protected def encode(target: Any, level: Int) = Try(tabulate(preferences.constants(terminal), level))
	}

	class AppendEncoder[T, S](left: => SourceEncoder[T], right: => SourceEncoder[S])(implicit preferences: SourceViewPreferences) extends SourceEncoder[(T, S)] {
		protected def encode(target: (T, S), level: Int) = for {
			previous <- left(target._1, level)
			next <- right(target._2)
		} yield previous ++ next
	}

	class TransformEncoder[T, S](before: => SourceEncoder[S])(f: T => S)(implicit preferences: SourceViewPreferences) extends SourceEncoder[T] {
		protected def encode(target: T, level: Int) = for {
			next <- before(f(target), level)
		} yield next
	}

	class OrEncoder[T, -L <: T, -R <: T](left: => SourceEncoder[L], right: => SourceEncoder[R])(implicit preferences: SourceViewPreferences) extends SourceEncoder[T] {
		protected def encode(target: T, level: Int) =
			Try(target.asInstanceOf[L]).flatMap{ left(_, level) } orElse Try(target.asInstanceOf[R]).flatMap{ right(_, level) }
	}

	class RepeatEncoder[-T](body: => SourceEncoder[T], separator: => SourceEncoder[Null])(implicit preferences: SourceViewPreferences) extends SourceEncoder[Seq[T]] {
		protected def encode(target: Seq[T], level: Int) = {
			val sortedTarget = target //TODO!!  preferences.sortOrder(this).fold(target){ target.sortWith(_) }
			if (sortedTarget.isEmpty) Try("")
			else ((body(sortedTarget.head, level), sortedTarget.head) /: sortedTarget.tail){
				case ((previous, previousElem), elem) =>
					(for {
						previous <- previous
						separator <- separator(null)
						elemBody <- body(elem, level)
					} yield previous ++ separator.dropReferences /*TODO!!++ preferences.lineBreak(InBetween(this) on (previousElem, elem, sortedTarget))*/ ++ elemBody, elem)
			}._1
		}
	}
}
