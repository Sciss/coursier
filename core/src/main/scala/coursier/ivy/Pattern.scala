package coursier.ivy

import fastparse.all._

import scala.language.{implicitConversions, reflectiveCalls}

final case class PropertiesPattern(chunks: Seq[PropertiesPattern.ChunkOrProperty]) {

  def string: String = chunks.map(_.string).mkString

  import PropertiesPattern.ChunkOrProperty

  def substituteProperties(properties: Map[String, String]): Either[String, Pattern] = {

    val validation: Either[Seq[String], Seq[Pattern.Chunk]] = {
      val (err1, ch1) = ((Seq.empty[String], Seq.empty[Pattern.Chunk]) /: chunks) { case ((err0, ch0), c) =>
        c match {
          case ChunkOrProperty.Prop(name, alternativesOpt) =>
            properties.get(name) match {
              case Some(value) =>
                (err0, ch0 :+ Pattern.Chunk.Const(value))
              case None =>
                alternativesOpt match {
                  case Some(alt) =>
                    val either = PropertiesPattern(alt)
                      .substituteProperties(properties)
                      .right.map(_.chunks)
                    either.fold[(Seq[String], Seq[Pattern.Chunk])](x => (err0 :+ x, ch0), xs => (err0, ch0 ++ xs))

                  case None =>
                    (err0 :+ name, ch0)
                }
            }

          case ChunkOrProperty.Opt(l @ _*) =>
            val either = PropertiesPattern(l)
              .substituteProperties(properties)
              .right.map(l => Vector(Pattern.Chunk.Opt(l.chunks: _*)))
            either.fold[(Seq[String], Seq[Pattern.Chunk])](x => (err0 :+ x, ch0), xs => (err0, ch0 ++ xs))

          case ChunkOrProperty.Var(name) =>
            (err0, ch0 :+ Pattern.Chunk.Var(name))

          case ChunkOrProperty.Const(value) =>
            (err0, ch0 :+ Pattern.Chunk.Const(value))
        }
      }

      if (err1.isEmpty) Right(ch1) else Left(err1)
    }

    validation.fold[Either[String, Pattern]]({ notFoundProps =>
      Left(s"Property(ies) not found: ${notFoundProps.toList.mkString(", ")}")
    }, { chunks =>
      Right(Pattern(chunks))
    })
  }
}

final case class Pattern(chunks: Seq[Pattern.Chunk]) {

  def +:(chunk: Pattern.Chunk): Pattern = Pattern(chunk +: chunks)

  import Pattern.Chunk

  def string: String = chunks.map(_.string).mkString

  def substituteVariables(variables: Map[String, String]): Either[String, String] = {

    def helper(chunks: Seq[Chunk]): Either[Seq[String], Seq[Chunk.Const]] = {
      val (err1, ch1) = ((Seq.empty[String], Seq.empty[Chunk.Const]) /: chunks) { case ((err0, ch0), c) =>
        c match {
          case Chunk.Var(name) =>
            variables.get(name) match {
              case Some(value) =>
                (err0, ch0 :+ Chunk.Const(value))
              case None =>
                (err0 :+ name, ch0)
            }
          case Chunk.Opt(l @ _*) =>
            val res = helper(l)
            val resV = res.fold[Seq[Chunk.Const]](_ => Nil, identity)
            (err0, ch0 ++ resV)
          case c: Chunk.Const =>
            (err0, ch0 :+ c)
        }
      }
      if (err1.isEmpty) Right(ch1) else Left(err1)
    }

    val validation = helper(chunks)

    validation match {
      case Left(notFoundVariables) =>
        Left(s"Variables not found: ${notFoundVariables.toList.mkString(", ")}")
      case Right(constants) =>
        val b = new StringBuilder
        constants.foreach(b ++= _.value)
        Right(b.result())
    }
  }
}

object PropertiesPattern {

  sealed abstract class ChunkOrProperty extends Product with Serializable {
    def string: String
  }

  object ChunkOrProperty {
    final case class Prop(name: String, alternative: Option[Seq[ChunkOrProperty]]) extends ChunkOrProperty {
      def string: String =
      s"$${" + name + alternative.fold("")(alt => "-" + alt.map(_.string).mkString) + "}"
    }
    final case class Var(name: String) extends ChunkOrProperty {
      def string: String = "[" + name + "]"
    }
    final case class Opt(content: ChunkOrProperty*) extends ChunkOrProperty {
      def string: String = "(" + content.map(_.string).mkString + ")"
    }
    final case class Const(value: String) extends ChunkOrProperty {
      def string: String = value
    }

    implicit def fromString(s: String): ChunkOrProperty = Const(s)
  }

  private object Parser {

    private val notIn         = s"[]{}()$$".toSet
    private val chars         = P(CharsWhile(c => !notIn(c)).!)
    private val noHyphenChars = P(CharsWhile(c => !notIn(c) && c != '-').!)

    private val constant      = P(chars).map(ChunkOrProperty.Const)

    private lazy val property: Parser[ChunkOrProperty.Prop] =
      P(s"$${" ~ noHyphenChars ~ ("-" ~ chunks).? ~ "}")
        .map { case (name, altOpt) => ChunkOrProperty.Prop(name, altOpt) }

    private lazy val variable: Parser[ChunkOrProperty.Var] = P("[" ~ chars ~ "]").map(ChunkOrProperty.Var)

    private lazy val optional: Parser[ChunkOrProperty.Opt] = P("(" ~ chunks ~ ")")
      .map(l => ChunkOrProperty.Opt(l: _*))

    lazy val chunks: Parser[Seq[ChunkOrProperty]] = P((constant | property | variable | optional).rep)
      .map(_.toVector) // "Vector" is more readable than "ArrayBuffer"
  }

  def parser: Parser[Seq[ChunkOrProperty]] = Parser.chunks


  def parse(pattern: String): Either[String, PropertiesPattern] =
    parser.parse(pattern) match {
      case f: Parsed.Failure =>
        Left(f.msg)
      case Parsed.Success(v, _) =>
        Right(PropertiesPattern(v))
    }

}

object Pattern {

  sealed abstract class Chunk extends Product with Serializable {
    def string: String
  }

  object Chunk {
    final case class Var(name: String) extends Chunk {
      def string: String = "[" + name + "]"
    }
    final case class Opt(content: Chunk*) extends Chunk {
      def string: String = "(" + content.map(_.string).mkString + ")"
    }
    final case class Const(value: String) extends Chunk {
      def string: String = value
    }

    implicit def fromString(s: String): Chunk = Const(s)
  }

  import Chunk.{Opt, Var}

  // Corresponds to
  //   [organisation]/[module]/(scala_[scalaVersion]/)(sbt_[sbtVersion]/)[revision]/[type]s/[artifact](-[classifier]).[ext]

  val default = Pattern(
    Seq(
      Var("organisation"), "/",
      Var("module"), "/",
      Opt("scala_", Var("scalaVersion"), "/"),
      Opt("sbt_", Var("sbtVersion"), "/"),
      Var("revision"), "/",
      Var("type"), "s/",
      Var("artifact"), Opt("-", Var("classifier")), ".", Var("ext")
    )
  )

}