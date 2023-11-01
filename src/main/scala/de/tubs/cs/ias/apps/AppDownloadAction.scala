package de.tubs.cs.ias.apps

import de.tubs.cs.ias.OperatingSystem.OperatingSystem
import de.tubs.cs.ias.applist.MobileApp
import de.tubs.cs.ias.apps.android.{Panic, Result, Success, AppDownloader => AndroidAppDownloader}
import de.tubs.cs.ias.apps.ios.{AppDownloader => IosAppDownloader}
import de.tubs.cs.ias.apps.fdroid.{AppDownloader => FDroidAppDownloader}
import de.tubs.cs.ias.util.{ActionReport, AsciiProgressBar, Config}
import wvlet.log.LogSupport
import java.io.File
import scala.collection.mutable.{Map => MMap}

object AppDownloadAction extends LogSupport {

  def download(
      apps: List[MobileApp],
      folder: String,
      conf: Config,
      os: OperatingSystem
  ): ActionReport = {
    val bar = AsciiProgressBar.create("Downloading Apps  ", apps.length.toLong)
    val failures = MMap[String, String]()
    try {
      apps.foreach { app =>
        try {
          val id = app.bundleId
          os match {
            case de.tubs.cs.ias.OperatingSystem.ANDROID =>
              if (!new File(s"$folder/$id.apk").exists()) { // this check is currently pointless as the version is
                // appended
                AndroidAppDownloader.download(id, folder, conf.android) match {
                  case x:
                    Panic =>
                    error(s"$id -> ${x.getMessage}")
                    failures.addOne(id -> x.getMessage)
                  case Result(_) =>
                    throw new RuntimeException(
                      "download does not return a result"
                    )
                  case Success =>
                }
              }
            case de.tubs.cs.ias.OperatingSystem.IOS =>
              if (!new File(s"$folder/$id.ipa").exists()) {
                IosAppDownloader.downloadAppUsingIpatoolPy(
                  id,
                  folder,
                  conf.ios.login,
                  conf.ios.ipatoolpy
                ) match {
                  case Some(value) =>
                    error(id -> value)
                    failures.addOne(id -> value)
                  case None =>
                }
              }
            case de.tubs.cs.ias.OperatingSystem.FDROID =>
              val version = if (app.version.toInt != 0) s"_${app.version}" else ""
              if (!new File(s"$folder/$id$version.apk").exists()) {
                FDroidAppDownloader.downloadApk(
                  id,
                  folder,
                  conf.fdroid.fdroidcl,
                  app.version.toInt
                ) match {
                  case x: FDroidAppDownloader.Panic =>
                    error(s"$id -> ${x.getMessage}")
                    failures.addOne(id -> x.getMessage)
                  case FDroidAppDownloader.Success =>
                }
              }
          }
        } catch {
          case e: Exception =>
            val id = app.bundleId
            error(
              s"$id -> ${e.getMessage} \n ${e.getStackTrace.mkString("\n")}"
            )
            failures.addOne(id -> e.getMessage)
          case e: Error =>
            val id = app.bundleId
            error(s"$id -> ${e.getMessage}")
            failures.addOne(id -> e.getMessage)
        } finally {
          bar.step()
        }
      }
    } finally {
      bar.close()
    }
    ActionReport(apps.length - failures.size, failures.size, failures.toMap)
  }

}
