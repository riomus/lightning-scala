package org.viz.lightning

import org.apache.toree.magic.MagicManager
import org.json4s.DefaultFormats
import org.json4s.native.Serialization

import scala.language.dynamics
import scala.util.Random
import scalaj.http._

class Visualization(val lgn: Lightning, val id: String, val name: String) {
  def show()(implicit magicManager:MagicManager)={
    val id=Random.alphanumeric.take(10).mkString("")
    magicManager.html(
      s"""
<script type="application/javascript">
window.addEventListener('DOMContentReady', function(e) {
    var iFrame = document.getElementById( '${id}' );
    iFrame.width  = iFrame.contentWindow.document.body.scrollWidth;
    iFrame.height = iFrame.contentWindow.document.body.scrollHeight;
    window.removeEventListener('DOMContentReady',this)
} );

</script>
         <iframe src="${this.getIframeLink}" width="100%" height="900px" id="${id}"  frameBorder="0">
         </iframe>
       """
  )
  }

  def formatURL(url: String): String = {
    val out = url.last.toString match {
      case "/" => url
      case _ => url + "/"
    }
    out + "?host=" + lgn.host
  }

  def getPermalinkURL: String = {
    lgn.host + "/visualizations/" + id
  }

  def getEmbedLink: String = {
    formatURL(this.getPermalinkURL + "/embed")
  }

  def getIframeLink: String = {
    formatURL(this.getPermalinkURL + "/iframe")
  }

  def getPymLink: String = {
    formatURL(this.getPermalinkURL + "/pym")
  }

  def getDataLink: String = {
    formatURL(lgn.host + "/sessions/" + lgn.session + "/visualizations/" + id + "/data/")
  }

  def getHTML: String = {
    val url = getEmbedLink
    var request = Http(url).method("GET")
    if (lgn.auth.nonEmpty) {
      request = request.auth(lgn.auth.get._1, lgn.auth.get._2)
    }
    request.asString.body
  }

  def append(payload: Map[String, Any]) : Visualization = {
    val url = lgn.host + "/sessions/" + lgn.session + "/visualizations/" + this.id + "/data/"
    implicit val formats = DefaultFormats
    val blob = Map("data" -> payload)
    lgn.post(url, Serialization.write(blob))
    this
  }
	
  def getPublicLink: String = {
    this.getPermalinkURL + "/public/"
  }
  
}
