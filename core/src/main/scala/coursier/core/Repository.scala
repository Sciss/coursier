package coursier.core

import coursier.Fetch

import scala.concurrent.Future
import scala.language.higherKinds

trait Repository extends Product with Serializable {
  def find(
    module: Module,
    version: String,
    fetch: Fetch.Content
  ): Future[Either[String, (Artifact.Source, Project)]]
}

object Repository {

  implicit class ArtifactExtensions(val underlying: Artifact) extends AnyVal {
    def withDefaultChecksums: Artifact =
      underlying.copy(checksumUrls = underlying.checksumUrls ++ Seq(
        "MD5" -> (underlying.url + ".md5"),
        "SHA-1" -> (underlying.url + ".sha1")
      ))
    def withDefaultSignature: Artifact =
      underlying.copy(extra = underlying.extra ++ Seq(
        "sig" ->
          Artifact(
            underlying.url + ".asc",
            Map.empty,
            Map.empty,
            Attributes("asc", ""),
            changing = underlying.changing,
            authentication = underlying.authentication
          )
            .withDefaultChecksums
      ))
  }
}

