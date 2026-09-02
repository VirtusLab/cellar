package cellar

import coursierapi.{MavenRepository, Repository}

private[cellar] object ExtraRepositories:

  /** Configured repositories first, command-line `-r` values appended.
    *
    * Duplicates collapse to their first occurrence, ignoring a trailing slash.
    */
  def effective(configured: List[MavenRepository], commandLine: List[MavenRepository]): List[Repository] =
    (configured ++ commandLine).distinctBy(_.getBase.stripSuffix("/"))
