package cellar

object MetaFormatter:
  def format(meta: PomMeta): String =
    val lines = List.newBuilder[String]

    meta.name.foreach(v => lines += s"Name:        $v")
    meta.description.foreach(v => lines += s"Description: $v")
    meta.url.foreach(v => lines += s"URL:         $v")

    meta.licenses.foreach { lic =>
      val rendered = lic.url.fold(lic.name)(u => s"${lic.name} ($u)")
      lines += s"License:     $rendered"
    }

    meta.scm.foreach(v => lines += s"SCM:         $v")

    if meta.developers.nonEmpty then
      lines += "Developers:"
      meta.developers.foreach { dev =>
        val rendered = dev.email.fold(dev.name)(e => s"${dev.name} <$e>")
        lines += s"  - $rendered"
      }

    lines.result().mkString("\n")
