package de.tubs.cs.ias.apps.android

import de.tubs.cs.ias.util.Config
import java.util.Date

case class PlayAppVersion(code: Long, name: String)

case class PlayAppInfo(downloads: String,
                       files: List[String],
                       name: String,
                       developer: String,
                       price: Double,
                       minAndroidVersion: String,
                       size: String,
                       updated: Option[Date],
                       version: PlayAppVersion)
