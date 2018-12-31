package waff.dojo

trait OrderJob {

  import OrderJob._

  val ordered: (String) => String = parse _ andThen resolveDependency

  def parse(input: String): List[Job] = {
    val validChars = '|' :: ('a' to 'z').toList
    val cleared    = input filter (c => validChars contains c)

    (cleared split '|').toList filterNot (_.isEmpty) map (f => Job(f.head, f.tail.lastOption))
  }

  def resolveDependency(jobs: List[Job]): String = {

    def acc(jobs: List[Job], result: String): String = jobs.partition(_.dependency.isEmpty) match {
      case (resolved, Nil)        => result + (resolved map (_.id) mkString)
      case (Nil, unresolved)      => "circular dependency"
      case (resolved, unresolved) =>
        val resolvedIds = resolved map (_.id)
        acc(unresolved map (unResolveDependency(_, resolvedIds)), result + resolvedIds.mkString)
    }

    jobs.find(_.hasSelfDependency).fold(acc(jobs, ""))(_ => "self dependency")
  }

  def unResolveDependency(jd: Job, resolved: List[Char]) = Job(jd.id, jd.dependency filterNot (resolved contains _))
}

object OrderJob extends OrderJob {
  case class Job(id: Char, dependency: Option[Char]) {
    val hasSelfDependency = dependency.fold(false)(_ == id)
  }
}
