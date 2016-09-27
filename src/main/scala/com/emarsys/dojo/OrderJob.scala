package com.emarsys.dojo

object OrderJob {

  def order(input: String): String = {
    val jobDependencies = parse(input)

    if (jobDependencies.forall(_.noSelfDependency)) resolveDependency(jobDependencies).map(_.job).mkString("")
    else throw new Exception("self dependency")
  }

  def resolveDependency(jobs: List[JobDependency]): List[JobDependency] = jobs.partition(_.dependency.isEmpty) match {
    case (noDependency, Nil)            => noDependency
    case (Nil, withDependency)          => throw new Exception("circular dependency")
    case (noDependency, withDependency) => noDependency ++ resolveDependency(withDependency map {
      w => removeResolvedDependency(w, noDependency.map(_.job))
    })
  }

  def removeResolvedDependency(jd: JobDependency, resolved: List[Char]) =
    JobDependency(jd.job, jd.dependency.filterNot(resolved.contains(_)))

  def parse(input: String): List[JobDependency] = {
    if (input.length < 1) List.empty[JobDependency]
    else input.split('|').toList.map(f => JobDependency(f.head, if (f.last == '>') None else f.lastOption))
  }

  case class JobDependency(job: Char, dependency: Option[Char]) {
    val noSelfDependency = dependency.fold(true)(_ != job)
  }
}
