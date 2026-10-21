package cellar

import coursier.{MavenRepository, Repository}

private[cellar] object ExtraRepositories:

  /** Configured repositories first, command-line `-r` values appended.
    *
    * Duplicates collapse to the position of their first occurrence, ignoring a trailing slash. An
    * occurrence carrying credentials wins over one without, so `-r https://user:token@host/...` is
    * not lost to the same URL already configured without them.
    */
  def effective(configured: List[MavenRepository], commandLine: List[MavenRepository]): List[Repository] =
    val all = configured ++ commandLine
    def key(repo: MavenRepository) = repo.root.stripSuffix("/")
    all.distinctBy(key).map { first =>
      all.find(repo => key(repo) == key(first) && repo.authentication.nonEmpty).getOrElse(first)
    }
