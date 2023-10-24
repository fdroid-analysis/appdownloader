package de.tubs.cs.ias.applist

import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import java.io.File
import spray.json._
import wvlet.log.LogSupport

object AppListParser extends DefaultJsonProtocol with LogSupport {

  implicit val mobileAppFormat: RootJsonFormat[MobileApp] = jsonFormat5(
    MobileApp)

  implicit val appListFormat: RootJsonFormat[MobileAppList] = jsonFormat2(
    MobileAppList)

  def read(path: String): MobileAppList = {
    JsonParser(fsi.readInTextFile(path)).convertTo[MobileAppList]
  }

  def write(appList: MobileAppList, path: String): Unit = {
    if (!new File(path).exists()) fsi.writeFile(appList.toJson.prettyPrint, path)
  }
}
