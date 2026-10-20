package cellar

import coursier.cache.FileCache
import coursier.util.Task

private[cellar] object CoursierCache:

  /** The cache every coursier call shares.
    *
    * Some feeds (Azure Artifacts) answer an anonymous request with `203` and a sign-in page instead
    * of challenging it. Accepted, that page would be cached as the artifact and break every later
    * lookup, so it fails the download instead.
    */
  lazy val instance: FileCache[Task] = FileCache().copy(rejectNonAuthoritativeResponses = true)
