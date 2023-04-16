package handrail

import cats.syntax.all._
import cats.parse.{Parser0 => P0, Parser => P, Numbers, Rfc5234}

import handrail.model._
import cats.data.Chain

/**   - {{ 'foo' }} -> render(ctx.withValue("foo"))
  *   - {{ this }} -> render(this(ctx))
  *   - {{ . }} -> render(this(ctx))
  *   - {{ foo }} -> render(foo(ctx) | lookup(this(), "foo"))
  *   - {{ foo 'foo'}} -> render(foo('foo'))
  *   - {{ ./foo }} -> render(lookup(this(), "foo"))
  *   - {{#foo a b c}}bar{{/foo }}-> render(foo(a, b, b, body="bar"))
  */
class PropertyParserSuite extends munit.FunSuite {

  sealed trait Property
  object Property {
    case object Parent extends Property
    case object This extends Property
    case class Name(value: String) extends Property
    case class Index(value: Int) extends Property
  }

  val OpenSquareBrace = P.char('[').void
  val CloseSquareBrace = P.char(']').void
  val Slash = P.char('/')
  val Dot = P.char('.')
  val Space = P.char(' ')

  val parentP: P[Property] = (Dot ~ Dot).as(Property.Parent)
  val thisP: P[Property] = Dot.as(Property.This)

  val parentOrThisP = (Dot.as(Property.This) ~ Dot.as(Property.Parent).?).map { case (x, y) =>
    y.getOrElse(x)
  }

  val identifierP = (Rfc5234.alpha ~ (Rfc5234.alpha | Rfc5234.digit).rep0)
    .map { case (head, tail) =>
      (head :: tail).mkString
    }
    .withContext("identifierP")

  val propertyP: P[Property] =
    P.oneOf(
      List(
        (OpenSquareBrace *> (Numbers.nonNegativeIntString
          .map(v => Property.Index(v.toInt)) | P.until(CloseSquareBrace).map(Property.Name(_))) <* CloseSquareBrace),
        identifierP.map(Property.Name(_))
      )
    ).withContext("propertyP")

  lazy val propertyExp: P[Chain[Property]] =
    (propertyP ~ ((Dot *> P.defer(propertyExp) | (Slash *> P.defer(expP)))).?)
      .map { case (head, tail) =>
        tail.getOrElse(Chain.empty).append(head)
      }
      .withContext("propertyExp")

  lazy val parentOrThisExp: P[Chain[Property]] =
    (parentOrThisP ~ (Slash *> P.defer(expP).?))
      .map { case (head, tail) =>
        tail.getOrElse(Chain.empty).append(head)
      }
      .withContext("parentOrThisExp")

  lazy val expP: P[Chain[Property]] = (parentOrThisExp | propertyExp).withContext("expP")

  test("propertyP should parse {{foo}}") {

    val source = "foo"

    val result = propertyP.parseAll(source)

    assertEquals(result, Right(Property.Name("foo")))
  }

  test("propertyP should parse {{[foo]}}") {

    val source = "[foo]"

    val result = propertyP.parseAll(source)

    assertEquals(result, Right(Property.Name("foo")))
  }

  test("propertyP should no parse {{'foo'}}") {

    val source = "'foo'"

    val result = propertyP.parseAll(source)

    assert(result.isLeft)
  }

  test("propertyP should no parse {{\"foo\"}}") {

    val source = "\"foo\""

    val result = propertyP.parseAll(source)

    assert(result.isLeft)
  }

  test("expP should parse {{foo.bar}}") {

    val source = "foo.bar"

    val result = expP.parseAll(source)

    assertEquals(
      result,
      Right(Chain(Property.Name("bar"), Property.Name("foo")))
    )
  }

  test("expP should parse {{foo.[0].bar}}") {

    val source = "foo.[0].bar"

    val result = expP.parseAll(source)

    assertEquals(
      result,
      Right(Chain(Property.Name("bar"), Property.Index(0), Property.Name("foo")))
    )
  }

  test("expP should parse {{foo/bar}}") {

    val source = "foo/bar"

    val result = expP.parseAll(source)

    assertEquals(
      result,
      Right(Chain(Property.Name("bar"), Property.Name("foo")))
    )
  }

  test("expP should parse {{foo.bar.baz}}") {

    val source = "foo.bar.baz"

    val result = expP.parseAll(source)

    assertEquals(result, Right(Chain("baz", "bar", "foo").map(Property.Name.apply)))
  }

  test("expP should parse {{foo.[bar].baz}}") {

    val source = "foo.[bar].baz"

    val result = expP.parseAll(source)

    assertEquals(result, Right(Chain("baz", "bar", "foo").map(Property.Name.apply)))
  }

  test("expP should parse {{../foo.[bar].baz}}") {

    val source = "../foo.[bar].baz"

    val result = expP.parseAll(source)

    assertEquals(
      result,
      Right(Chain(Property.Name("baz"), Property.Name("bar"), Property.Name("foo"), Property.Parent))
    )
  }

  test("expP should parse {{../foo/[bar].baz}}") {

    val source = "../foo/[bar].baz"

    val result = expP.parseAll(source)

    assertEquals(
      result,
      Right(Chain(Property.Name("baz"), Property.Name("bar"), Property.Name("foo"), Property.Parent))
    )
  }

  test("expP should parse {{../foo/../foo/./[bar].baz}}") {

    val source = """../foo/../foo/./[bar].baz"""

    val result = expP.parseAll(source)

    assertEquals(
      result,
      Right(
        Chain(
          Property.Name("baz"),
          Property.Name("bar"),
          Property.This,
          Property.Name("foo"),
          Property.Parent,
          Property.Name("foo"),
          Property.Parent
        )
      )
    )
  }

}
