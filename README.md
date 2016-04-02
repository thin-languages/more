# More

[![Build Status](https://travis-ci.org/thin-languages/More.svg?branch=master)](https://travis-ci.org/thin-languages/More)
[![Stories in Ready](https://badge.waffle.io/thin-languages/more.svg?label=help%20wanted&title=Ready)](http://waffle.io/thin-languages/more)

A Scala based framework for developing modular [Thin-Languages](thin-languages.uqbar.org)

##Architecture Basics
Instead of defining a language as a single monolithic unit which comprises all functional abstractions along with syntax and
semanthics, *More* proposes a modular aproach: Turn every feature of your language into a separate module and compose them on
the run!

###Modules
You can think of each concept available on a language as a separate feature, that can be model as an independent module (For
example, *booleans*, *integers*, *objects* and *functions* can be isolated modules. More abstract tools like *default
parameters*, *inheritance* and *destructive asignation* can be modules too).

Separating the language notions into modules allow for easier testing and feature flexibility.

Instead of picking a langueage based on it's features, you can pick whatever features you want and combine them for making
the language you want.

To declare a *Language Module* on More, simply create a `trait` and define inside it any structure you need to support the
new concepts. All concepts should extend the `LanguageConcept` trait or another, higher order, concept.

```scala
trait YourModule {
	trait YourNewConcept extends LanguageConcept
}
```

You can then declare a language by just combining any modules you want!

```scala
object YourLanguage extends Language
	with OneModule
	with OtherModule
```

Of course, some modules would need to use or redefine concepts presented by other modules. Scala language already provides an
easy way of handling this: just linearize your traits as needed.

```scala
trait YourModule extends SomeOtherModule {
	case class YourNewConcept(aField: SomeOtherModuleTypeConcept) extends SomeOtherModuleConcept
}
```

Remember that, in order to remain true to the Thin ideas, the language concepts themselves should only be the *abstract
representation of your language ideas*. That means that they should refrain from containing syntactic related elements. This
way they are easier to combine, and the syntax can be define for each user, or even create new *projective view* that
represent it in a whole different way, maybe without any code at all.

###Views
So where IS the syntax? Well, setting aside that Thin Languages don't really NEED to have ONE syntax, most language will
need a way to be written into (and read from) text. Of course, some languages may also want to work their concepts using
different, crazier interfaces, that could be implemented in all kinds of frontend technologies.

For this *More* defines the `LanguageView` trait.

Think of a View as a two-way route between your *Language Concepts* and some representation (text based or not) that a
frontend tool can use to render them and let a user interact with a program definition.

With this in mind, the `LanguageView` trait should be quite simple to understand:

```scala
trait LanguageView[ViewModel, ViewOptions, LanguageRequirement] {
	def encode(target: LanguageConcept)(implicit language: LanguageRequirement, options: ViewOptions): Try[ViewModel]
	def decode(target: ViewModel)(implicit language: LanguageRequirement,  options: ViewOptions): Try[LanguageConcept]
}
```

Where:
- `ViewModel` is the type the view knows how to render. E.g. the ViewModel of a very simple view that renders code could be
`String`, since it would take the object graph that represents your program and turn it into a code string and v.v.
- `ViewOptions` is the type of the structure used by the view to contain any preference the user may have for the
transformation.
E.g. for a source view you may like to tabulate with spaces of tabs, so that should be a field in your view
ViewOptions object.
- `LanguageRequirement` is a trait that any language that wants to be render by the View should implement. This provides the
view with a way to request any logic that may be needed to turn from your language's model to the view's model.
E.g. a source view may need a grammar definition to convert your language concepts into a source string. Instead of
implementing that itself (which would be very hard to do without deeply coupling it with some modules) it just declares it
as a needed piece of info that must be provided. This helps keeping the views simple and short.

Once this is clear, you can see that the `encode` and `decode` methods are just transformers that let you go from the
Models defined in your modules to a ViewModel instance and back again.

If any of this results a bit overwhelming, don't worry! Unless you want your model to be represented in a completely new way
you probably will have no need to define your own views. The good thing about keeping the views completely decoupled from
any LanguageConcept is that most common possible views should already be implemented and can be used with any language that
meets the requirements. One example of that is More's `SourceView` which should be able to turn any language into source
code, as long as they implement the `Sourceable` trait.

Remember, Views should only be defined if, for some reason, a frontend tool requires a different representation of your
language's programs.

###The Presenter Pattern
Let's say you have created some language modules and would like use them along with a view:

```scala
// Your Modules
trait FooModule {
  case class Foo(n: Int) extends LanguageConcept
}

trait BarModule {
  case object Bar extends LanguageConcept
}

//Your View
trait FancyView[
  FancyModel, // What the FancyView can turn your model from/into.
  FancyOptions, // Some preferences the view may have.
  Fanciable // The requirements the view impose to the language. This is stuff needed by the view to encode/decode.
] { ... }

trait Fanciable {
  def getTheFancy(target: LanguageConcept): Option[Fancy] = None
}

// Your Language
object YourLanguage extends Language
  with FooModule
  with BarModule
```

From the code you can tell the `FancyView` need the language to extend the `Fanciable` trait in order to work. As in
any other view, the LanguageRequirement traits are probably going to be the place where the hard stuff takes place.
Of course, you could design your modules to implement those requirements themselves, but that would make very
hard to implement a new module (since it would have to provide all logic needed by any views it would be used on). *More*
architecture is design to allow modules and views to be developed independently, completely decoupled from each other. We
think it is a good idea to allow modules to be removed or added any time from a language, without having to worry about
breaking the views.

Alternatively, you can just add a huge implementation of the language requirement in the language itself:

```scala
object YourLanguage extends Language with Fanciable
  with FooModule
  with BarModule {
  
  def getTheFancy(target: LanguageConcept) = target match {
    case Foo(n) => // Some Foo fancy
    case Bar => // Some Bar fancy
    case _ => super.getTheFancy(target)
  }
}
```

Althought this would kind of solve the problem it would be very repetitive, since every language that wants to render these
modules with this view would have to repeat this code (and probably extend it to cover all the modules it wants to use),
plus, the code would probably be very long and complex.

The proposal of *More*'s architecture is to apply a *Presenter Pattern*, creating a small trait that provides whatever a
single view needs for each module. That way each module becomes available of being render by a view (independently of
others) without becoming coupled with the view itself.

Here is how the final version would be:

```scala

trait FanciableFoo extends Fanciable {on: Foo =>
  def getTheFancy(target: LanguageConcept) = target match {
    case Foo(n) => // Some Foo fancy
    case _ => super.getTheFancy(target)
  }
}

trait FanciableBar extends Fanciable { on: Bar =>
  def getTheFancy(target: LanguageConcept) = target match {
    case Bar => // Some Bar fancy
    case _ => super.getTheFancy(target)
  }
}

object YourLanguage extends Language
  with FooModule with FanciableFoo
  with BarModule with FanciableBar
```

Notice how, if properly implemented, there would be no need for the language to declare any code, but it could be used,
as any other type, to solve conflicts between modules or presenters. Also notice that each presenter should only need to be
coded once, and be rehused by any language that wants to render with the FancyView, regardless of what other modules it
uses.

Presenters for popular views could be included in the modules packages themselves while presenters for new views could be
declared anywhere, without affecting languages that won't use them.

##Related Projects

For a growing sanitized set of modules that work well with each other you can check the [Less Thin-Language](https://github.com/thin-languages/Less)

We are also working on [Slimdown](https://github.com/thin-languages/Slimdown) a Thin-IDE, to provide desktop support to any *More* based language.

##Contributions

Sure! Pull requests are always welcome, just try to keep it small and clean. Also feel free to post any question or bug
report in the issue tracker.

##License

This code is open source software licensed under the [LGPL v3 License](https://www.gnu.org/licenses/lgpl.html) by [The Uqbar Foundation](http://www.uqbar-project.org/). Feel free to use it accordingly.
