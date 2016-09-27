package com.emarsys.dojo

object OrderJob {

  def order(input: String): String = pipeline(input)

  val pipeline: (String) => String = parse _ andThen checkSelfDependency andThen resolveDependency andThen extractIds andThen toString

  def parse(input: String): List[Job] =
    (clear(input) split '|').toList filterNot (_.isEmpty) map (f => Job(f.head, f.tail.lastOption))

  def checkSelfDependency(jobs: List[Job]): List[Job] =
    jobs.find(_.hasSelfDependency).fold(jobs)(_ => throw new Exception("self dependency"))

  def resolveDependency(jobs: List[Job]): List[Job] = jobs.partition(_.dependency.isEmpty) match {
    case (noDependency, Nil)            => noDependency
    case (Nil, withDependency)          => throw new Exception("circular dependency")
    case (noDependency, withDependency) => noDependency ++ resolveDependency(withDependency map { w =>
      removeResolvedDependency(w, extractIds(noDependency))
    })
  }

  def extractIds(jobs: List[Job]) = jobs map (_.id)
  def toString(ids: List[Char])   = ids mkString ""
  val validCharacters             = '|' :: ('a' to 'z').toList
  val clear                       = (s: String) => s filter (c => validCharacters contains c )
  val removeResolvedDependency    = (jd: Job, resolved: List[Char]) =>
    Job(jd.id, jd.dependency filterNot (resolved contains _))

  case class Job(id: Char, dependency: Option[Char]) {
    val hasSelfDependency = dependency.fold(false)(_ == id)
  }
}
