package cellar

object MetaFormatter:
  def format(meta: PomMeta): String =
    val lines = List.newBuilder[String]

    meta.name.foreach(v => lines += s"Name:        $v")
    meta.description.foreach(v => lines += s"Description: $v")
    meta.url.foreach(v => lines += s"URL:         $v")

    meta.licenses.foreach { (name, url) =>
      val rendered = url.fold(name)(u => s"$name ($u)")
      lines += s"License:     $rendered"
    }

    meta.scm.foreach(v => lines += s"SCM:         $v")

    if meta.developers.nonEmpty then
      lines += "Developers:"
      meta.developers.foreach { (name, email) =>
        val rendered = email.fold(name)(e => s"$name <$e>")
        lines += s"  - $rendered"
      }

    lines.result().mkString("\n")
