package de.tubs.cs.ias.applist

import de.tubs.cs.ias.applist.AppListParser.appListFormat
import spray.json.{JsonParser, enrichAny}
import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import java.io.File

case class MobileApp(
    name: String,
    bundleId: String,
    category: String,
    rank: Int,
    price: String,
    version: String = ""
)

case class MobileAppList(apps: List[MobileApp], name: String)
