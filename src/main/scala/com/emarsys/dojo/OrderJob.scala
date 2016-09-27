package com.emarsys.dojo

object OrderJob {

  def order(input: String): String = pipeline(input)

  val pipeline = parse _ andThen checkSelfDependency andThen resolveDependency andThen toString

  def parse(input: String) =
    (clear(input) split '|').toList filterNot (_.isEmpty) map (f => JobDependency(f.head, f.tail.lastOption))

  def checkSelfDependency(jobs: List[JobDependency]): List[JobDependency] =
    if (jobs.forall(_.noSelfDependency)) jobs
    else throw new Exception("self dependency")

  def resolveDependency(jobs: List[JobDependency]): List[JobDependency] = jobs.partition(_.dependency.isEmpty) match {
    case (noDependency, Nil)            => noDependency
    case (Nil, withDependency)          => throw new Exception("circular dependency")
    case (noDependency, withDependency) => noDependency ++ resolveDependency(withDependency map {
      w => removeResolvedDependency(w, noDependency map (_.job))
    })
  }

  def toString(jobs: List[JobDependency]) = jobs.map(_.job).mkString("")

  val removeResolvedDependency = (jd: JobDependency, resolved: List[Char]) =>
    JobDependency(jd.job, jd.dependency filterNot (resolved contains _))

  val clear = (s: String) => s filter (c => '|' :: ('a' to 'z').toList contains c)

  case class JobDependency(job: Char, dependency: Option[Char]) {
    val noSelfDependency = dependency.fold(true)(_ != job)
  }
}
