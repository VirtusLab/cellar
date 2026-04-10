package cellar

import org.w3c.dom.Element

import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory

final case class PomMeta(
    coordinate: MavenCoordinate,
    name: Option[String],
    description: Option[String],
    url: Option[String],
    licenses: Seq[(String, Option[String])],
    scm: Option[String],
    developers: Seq[(String, Option[String])],
)

object PomParser:
  def parse(path: fs2.io.file.Path, coord: MavenCoordinate): PomMeta =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
    factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_DTD, "")
    factory.setAttribute(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "")
    val doc  = factory.newDocumentBuilder().parse(path.toNioPath.toFile)
    val root = doc.getDocumentElement

    def nodeSeq(nl: org.w3c.dom.NodeList): Seq[org.w3c.dom.Node] =
      (0 until nl.getLength).map(nl.item)

    def text(tag: String): Option[String] =
      nodeSeq(root.getChildNodes)
        .collectFirst { case e: Element if e.getTagName == tag => e.getTextContent.trim }
        .filter(_.nonEmpty)

    def children(parentTag: String, childTag: String): Seq[Element] =
      val parents = root.getElementsByTagName(parentTag)
      if parents.getLength == 0 then Seq.empty
      else
        nodeSeq(parents.item(0).getChildNodes)
          .collect { case e: Element if e.getTagName == childTag => e }

    def elemText(el: Element, tag: String): Option[String] =
      val nodes = el.getElementsByTagName(tag)
      Option.when(nodes.getLength > 0)(nodes.item(0).getTextContent.trim).filter(_.nonEmpty)

    val licenses = children("licenses", "license").flatMap { el =>
      elemText(el, "name").map(name => (name, elemText(el, "url")))
    }

    val developers = children("developers", "developer").flatMap { el =>
      val name  = elemText(el, "name")
      val email = elemText(el, "email")
      (name orElse email).map(display => (display, if name.isDefined then email else None))
    }

    val scm =
      val scms = root.getElementsByTagName("scm")
      if scms.getLength == 0 then None
      else elemText(scms.item(0).asInstanceOf[Element], "url")

    PomMeta(
      coordinate = coord,
      name = text("name"),
      description = text("description"),
      url = text("url"),
      licenses = licenses,
      scm = scm,
      developers = developers,
    )
