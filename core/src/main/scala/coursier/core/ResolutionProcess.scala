package coursier
package core

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.language.higherKinds

sealed abstract class ResolutionProcess {
  def run(
    fetch: Fetch.Metadata,
    maxIterations: Int = 50
  ): Future[Resolution] = {

    if (maxIterations == 0) Future.successful(current)
    else {
      val maxIterations0 =
        if (maxIterations > 0) maxIterations - 1 else maxIterations

      this match {
        case Done(res) =>
          Future.successful(res)
        case missing0 @ Missing(missing, _, _) =>
          fetch(missing).flatMap(result =>
            missing0.next(result).run(fetch, maxIterations0)
          )
        case cont @ Continue(_, _) =>
          cont
            .nextNoCont
            .run(fetch, maxIterations0)
      }
    }
  }

  @tailrec
  final def next(
    fetch: Fetch.Metadata,
    fastForward: Boolean = true
  ): Future[ResolutionProcess] = {

    this match {
      case Done(_) =>
        Future.successful(this)
      case missing0 @ Missing(missing, _, _) =>
        fetch(missing).map(result => missing0.next(result))
      case cont @ Continue(_, _) =>
        if (fastForward)
          cont.nextNoCont.next(fetch, fastForward = fastForward)
        else
          Future.successful(cont.next)
    }
  }

  def current: Resolution
}

final case class Missing(
  missing: Seq[(Module, String)],
  current: Resolution,
  cont: Resolution => ResolutionProcess
) extends ResolutionProcess {

  def next(results: Fetch.MD): ResolutionProcess = {

    val errors = results.collect {
      case (modVer, Left(errs)) =>
        modVer -> errs
    }
    val successes = results.collect {
      case (modVer, Right(repoProj)) =>
        modVer -> repoProj
    }

    def cont0(res: Resolution): ResolutionProcess = {

      val depMgmtMissing0 = successes.map {
        case elem @ (_, (_, proj)) =>
          elem -> res.dependencyManagementMissing(proj)
      }

      val depMgmtMissing = depMgmtMissing0.map(_._2).fold(Set.empty)(_ ++ _) -- results.map(_._1)

      if (depMgmtMissing.isEmpty) {

        type Elem = ((Module, String), (Artifact.Source, Project))
        val modVer = depMgmtMissing0.map(_._1._1).toSet

        @tailrec
        def order(map: Map[Elem, Set[(Module, String)]], acc: List[Elem]): List[Elem] =
          if (map.isEmpty)
            acc.reverse
          else {
            val min = map.map(_._2.size).min // should be 0
            val (toAdd, remaining) = map.partition {
              case (_ /* k */, v) => v.size == min
            }
            val acc0 = toAdd.keys.foldLeft(acc)(_.::(_))
            val remainingKeys = remaining.keySet.map(_._1)
            val map0 = remaining.map {
              case (k, v) =>
                k -> v.intersect(remainingKeys)
            }
            order(map0, acc0)
          }

        val orderedSuccesses = order(depMgmtMissing0.map { case (k, v) => k -> v.intersect(modVer) }.toMap, Nil)

        val res0 = orderedSuccesses.foldLeft(res) {
          case (acc, (modVer1, (source, proj))) =>
            acc.copyWithCache(
              projectCache = acc.projectCache + (
                modVer1 -> (source, acc.withDependencyManagement(proj))
              )
            )
        }

        Continue(res0, cont)
      } else
        Missing(depMgmtMissing.toSeq, res, cont0)
    }

    val current0 = current.copyWithCache(
      errorCache = current.errorCache ++ errors
    )

    cont0(current0)
  }

}

final case class Continue(
  current: Resolution,
  cont: Resolution => ResolutionProcess
) extends ResolutionProcess {

  def next: ResolutionProcess = cont(current)

  @tailrec def nextNoCont: ResolutionProcess =
    next match {
      case nextCont: Continue => nextCont.nextNoCont
      case other => other
    }

}

final case class Done(resolution: Resolution) extends ResolutionProcess {
  def current: Resolution = resolution
}

object ResolutionProcess {
  def apply(resolution: Resolution): ResolutionProcess = {
    val resolution0 = resolution.nextIfNoMissing

    if (resolution0.isDone)
      Done(resolution0)
    else
      Missing(resolution0.missingFromCache.toSeq, resolution0, apply)
  }
}

