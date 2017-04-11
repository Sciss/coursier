package coursier

import scala.concurrent.{ExecutionContext, Future}
import scala.language.higherKinds

object Fetch {

  type Content = Artifact => Future[Either[String, String]]

  type MD = Seq[(
    (Module, String),
    Either[Seq[String], (Artifact.Source, Project)]
  )]

  type Metadata = Seq[(Module, String)] => Future[MD]

  /**
    * Try to find `module` among `repositories`.
    *
    * Look at `repositories` from the left, one-by-one, and stop at first success.
    * Else, return all errors, in the same order.
    *
    * The `version` field of the returned `Project` in case of success may not be
    * equal to the provided one, in case the latter is not a specific
    * version (e.g. version interval). Which version get chosen depends on
    * the repository implementation.
    */
  def find(
    repositories: Seq[Repository],
    module: Module,
    version: String,
    fetch: Content
  )(implicit exec: ExecutionContext): Future[Either[Seq[String], (Artifact.Source, Project)]] = {

    val lookups: Seq[(Repository, Future[Either[String, (Artifact.Source, Project)]])] =
      repositories.map(repo => repo -> repo.find(module, version, fetch) /* .run */)

    val task = lookups.foldLeft[Future[Either[Seq[String], (Artifact.Source, Project)]]](Future.successful(Left(Nil))) {
      case (acc, (_ /* repo */, eitherProjTask)) =>
        acc.flatMap {
          case Left(errors) =>
            eitherProjTask.map(_.left.map(error => error +: errors))
          case res @ Right(_) =>
            Future.successful(res)
        }
    }

    task.map(_.left.map(_.reverse))
  }

  def from(
    repositories: Seq[core.Repository],
    fetch: Content,
    extra: Content*
  ): Metadata = {
    ???
//    modVers: Seq[(Module, String)] =>
//      (
//        /* F.gatherUnordered( */
//          modVers.map { case (module, version) =>
//
//            def get(fetch: Content): Future[Either[Seq[String], (Artifact.Source, Project)]] =
//              find(repositories, module, version, fetch)
//
//            ((get(fetch) /: extra)((fut, f) =>  _ orElse get(f))).map((module, version) -> _)
//          }
//        /* ) */
//      ).map(_.toSeq)
  }
}