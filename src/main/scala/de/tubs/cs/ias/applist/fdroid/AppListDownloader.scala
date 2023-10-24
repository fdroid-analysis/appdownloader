package de.tubs.cs.ias.applist.fdroid

import de.tubs.cs.ias.applist.AppListParser.appListFormat
import de.tubs.cs.ias.applist.MobileAppList
import de.tubs.cs.ias.applist.fdroid.AppListParser.{readInRepoIndex => fdroidReadInRepoIndex}
import de.tubs.cs.ias.util.FileSystemInteraction
import dispatch.Defaults._
import dispatch._
import spray.json.{JsonParser, enrichAny}
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object AppListDownloader {

  private val FDROID_REPO_INDEX = "https://f-droid.org/repo/index-v2.json"

  def download(folder: String): MobileAppList = {
    val request     = url(FDROID_REPO_INDEX)
    val res: String = Await.result(Http.default(request OK as.String).either, Duration(5, TimeUnit.MINUTES)) match {
      case Left(value) => throw value
      case Right(value) => value
    }
    val indexFile   = s"$folder/index-v2.json"
    FileSystemInteraction.writeFile(res, indexFile)
    fdroidReadInRepoIndex(JsonParser(res))
  }

}
