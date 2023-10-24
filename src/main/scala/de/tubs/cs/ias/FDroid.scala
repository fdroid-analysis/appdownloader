package de.tubs.cs.ias

import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.OperatingSystems.FDROID
import de.tubs.cs.ias.Apmi.ensureFolderStructure
import de.tubs.cs.ias.applist.fdroid.{AppListParser => FDroidListParser}
import de.tubs.cs.ias.applist.fdroid.AppListParser.{
  Device,
  appCompatibleReason,
  getAppVersion
}
import de.tubs.cs.ias.applist.{
  AppListAction,
  AppListParser,
  MobileApp,
  MobileAppList
}
import de.tubs.cs.ias.apps.AppDownloadAction
import de.tubs.cs.ias.apps.fdroid.AppDownloader
import de.tubs.cs.ias.util.ActionReportJsonWriter.actionReportCollection
import de.tubs.cs.ias.util.{
  ActionReport,
  ActionReportCollection,
  Config,
  FileSystemInteraction => fsi
}
import scala.util.Success
import spray.json.enrichAny
import wvlet.log.LogSupport

object FDroid extends LogSupport {

  val parser: Parser = Parser("f-droid", "perform actions for f-droid")
    .addSubparser(
      Parser("download", "download apps, and privacy labels")
        .addFlag(
          "continue",
          "c",
          "continue",
          "if set the latest download will be resumed/restarted"
        )
        .addSubparser(
          Parser("app-list", "download the current repo index only")
            .addDefault[(ParsingResult, Config) => Unit](
              "func",
              this.downloadAppListAutomatedMain
            )
        )
        .addSubparser(
          Parser("apps", "download a given list of apps")
            .addSubparser(
              Parser("manual", "manually specify download parameters")
                .addPositional("folder", "the folder into which to download")
                .addPositional("list", "a txt file containing app ids")
            )
            .addSubparser(
              Parser("automated", "leverage the config for downloading apps")
                .addDefault[(ParsingResult, Config) => Unit](
                  "func",
                  this.downloadAppsAutomatedMain
                )
            )
        )
        .addSubparser(Parser("labels", "download a given list of labels"))
    )
    .addSubparser(
      Parser("verify", "sanity checks to verify everything is as it should be")
        .addPositional("list", "the list of apps to download")
        .addSubparser(
          Parser(
            "downloadComplete",
            "display number of downloaded apps and list missing apps"
          )
            .addPositional("folder", "the folder containing the apps")
            .addDefault[(ParsingResult, Config) => Unit](
              "func",
              this.verifyDownload
            )
        )
        .addSubparser(
          Parser(
            "downloadCompatible",
            "display number of compatible apps and list incompatible apps"
          )
            .addPositional("index", "the F-Droid index")
            .addPositional("api", "API level of the device")
            .addPositional(
              "abi",
              "list of comma-separated ABIs supported by the device, e.g. x86_64,arm64-v8a"
            )
            .addDefault[(ParsingResult, Config) => Unit](
              "func",
              this.verifyCompatible
            )
        )
    )
    .addSubparser(
      Parser(
        "full-chain",
        "do everything, download apps, download privacy labels"
      )
        .addDefault[(ParsingResult, Config) => Unit](
          "func",
          this.downloadAppsMain
        )
    )
    .addSubparser(
      Parser("util", "various utilities")
        .addSubparser(
          Parser(
            "listdiff",
            "displays the difference between two mobile app lists"
          )
            .addPositional("list1", "the first list")
            .addPositional("list2", "the second list")
            .addDefault[(ParsingResult, Config) => Unit]("func", this.listDiff)
        )
    )

  def downloadAppsMain(pargs: ParsingResult, conf: Config): Unit = {
    try {
      info("running full chain download")
      val (baseFolder, _, _, _) = ensureFolderStructure(
        conf.downloadFolderRoot + conf.fdroid.osFolderName + "/",
        pargs.getValue[Boolean]("continue")
      )
      val listReport: ActionReport =
        this.downloadAppListAutomatedMain(pargs, conf)
      val appsReport: ActionReport = this.downloadAppsAutomatedMain(pargs, conf)
      fsi.writeFile(
        ActionReportCollection(
          Map(
            "lists" -> listReport,
            "apps" -> appsReport
          )
        ).toJson.prettyPrint,
        s"$baseFolder/run.json"
      )
    } catch {
      case e: Error =>
        error(e.getMessage)
      case e: Exception =>
        error(e.getMessage)
    }
  }

  def downloadAppListAutomatedMain(
      pargs: ParsingResult,
      conf: Config
  ): ActionReport = {
    val (_, listsFolder, _, _) = ensureFolderStructure(
      conf.downloadFolderRoot + conf.fdroid.osFolderName + "/",
      pargs.getValue[Boolean]("continue")
    )
    info("starting chart list acquisition")
    val (appLists, report): (Map[String, MobileAppList], ActionReport) =
      AppListAction.download(FDROID, conf.fdroid.categories, listsFolder)
    appLists.foreach { case (category, list) =>
      info(s"category $category contains ${list.apps.length}")
    }
    info(
      s"overall we have ${appLists.flatMap(_._2.apps).toSet.size} unique apps"
    )
    report
  }

  def downloadAppsAutomatedMain(
      pargs: ParsingResult,
      conf: Config
  ): ActionReport = {
    AppDownloader.readinessCheckFDroidTool(conf.fdroid.fdroidcl)
    val (_, _, _, appsFolder) = ensureFolderStructure(
      conf.downloadFolderRoot + conf.fdroid.osFolderName + "/",
      pargs.getValue[Boolean]("continue")
    )
    info("starting automated app acquisition")
    val baseFolder = conf.downloadFolderRoot + conf.fdroid.osFolderName + "/20231012"
    val charts = conf.fdroid.categories.map(_.name)
    // val currentAppChartList = Apmi.getCurrentAppChartState(
    //   charts,
    //   conf.downloadFolderRoot + conf.fdroid.osFolderName + "/"
    // )
    // val allApps = currentAppChartList.flatMap(_._2.apps).toList
    val allApps =
      AppListParser.read(s"$baseFolder/lists/All.json").apps
    info(s"we have ${allApps.length} apps to download")
    val realAppsFolder = s"$baseFolder/apps"
    val remainingApps = getRemainingApps(allApps, realAppsFolder)
    info(s"we have ${remainingApps.size} remaining apps to download")
    AppDownloadAction.download(
      remainingApps.slice(0, List(allApps.length, conf.maxAmountApps).min),
      appsFolder,
      conf,
      FDROID
    )
  }

  private def getRemainingApps(
      allApps: List[MobileApp],
      appsFolder: String
  ): List[MobileApp] = {
    val downloadFolder = s"$appsFolder/fdroidcl/apks/"
    if (!fsi.fileExists(downloadFolder)) {
      return allApps
    }

    val collectedApps = fsi.getFiles(downloadFolder, Option(".apk"))
    if (collectedApps.nonEmpty) {
      val remainingAppIds = allApps.filterNot(app => {
        val prefix =
          if (app.version.toInt != 0) s"${app.bundleId}_${app.version}"
          else app.bundleId
        collectedApps.exists(fileName => fileName.startsWith(prefix))
      })
      info(
        s"${collectedApps.size} apps collected already: we have ${remainingAppIds.size} remaining apps to download"
      )
      remainingAppIds
    } else {
      allApps
    }
  }

  def verifyDownload(pargs: ParsingResult, conf: Config): ActionReport = {
    val appListPath = pargs.getValue[String]("list")
    val appFolderPath = pargs.getValue[String]("folder")
    val appsToDownload = AppListParser.read(appListPath).apps
    val appsDownloaded = fsi
      .getFiles(appFolderPath, Some(".apk"))
      .map { appPath => appPath.split('/').last }
      .map { appFileName =>
        {
          val split = appFileName.split('_')
          val versionCode = split.last.split('.')(0).toInt
          val appId = split.dropRight(1).mkString("_")
          (appId, versionCode)
        }
      }
    info(s"there are ${appsToDownload.size} apps on the list")
    // apps downloaded at all
    val downloaded = appsToDownload.filter { app =>
      appsDownloaded.map(_._1).contains(app.bundleId)
    }
    info(s"${downloaded.size} apps from the list are downloaded")
    // apps not downloaded
    val missing = appsToDownload.diff(downloaded)
    info(s"${missing.size} apps from the list are not downloaded")
    missing.foreach { app => println(app.bundleId) }
    val failedDownload = missing.map { appId =>
      appId.bundleId -> "app is not downloaded"
    }.toMap
    // apps downloaded, but wrong version
    val downloadVsListed = appsDownloaded.map { idVersion =>
      idVersion -> appsToDownload.find(_.bundleId == idVersion._1)
    }.toMap
    val wrongVersion = downloadVsListed
      .filter { idVersionApp =>
        val idVersion = idVersionApp._1
        val downloaded = idVersionApp._2
        if (downloaded.nonEmpty) {
          downloaded.get.version.toInt != idVersion._2
        } else {
          false
        }
      }
      .keys
      .toList
    val rightVersion = appsDownloaded
      .diff(wrongVersion)
      .filter { idVersion => appsToDownload.exists(_.bundleId == idVersion._1) }
    info(s"${rightVersion.size} apps are downloaded in the correct version")
    info(s"${wrongVersion.size} apps are downloaded in the wrong version")
    wrongVersion.foreach(idVersion => {
      val downloaded = downloadVsListed(idVersion).get
      println(
        s"${idVersion._1} (downloaded: ${idVersion._2}, should be ${downloaded.version})"
      )
    })
    val failedVersion = wrongVersion.map { idVersion =>
      idVersion._1 -> s"wrong version: ${idVersion._2}"
    }.toMap
    val fails = failedDownload ++ failedVersion
    ActionReport(downloaded.size, missing.size, fails)
  }

  def verifyCompatible(pargs: ParsingResult, conf: Config): ActionReport = {
    val appList = AppListParser.read(pargs.getValue[String]("list"))
    val index = FDroidListParser.readIndex(pargs.getValue[String]("index"))
    val api = pargs.getValue[String]("api").toInt
    val abis = pargs.getValue[String]("abi").split(',')
    val device = Device(abis, api)
    info(s"there are ${appList.apps.size} apps on the list")
    val versions = appList.apps.map { app =>
      app.bundleId -> getAppVersion(app.bundleId, app.version.toInt, index)
    }.toMap
    val versionsFound = versions
      .filter { idVersion => idVersion._2.nonEmpty }
      .map { idVersion => idVersion._1 -> idVersion._2.get }
    val appsVersionNotFound = appList.apps.filterNot { app =>
      versionsFound.keySet.contains(app.bundleId)
    }
    info(
      s"the index contains the corresponding version for ${versionsFound.size} apps"
    )
    info(
      s"the index does not contain the corresponding version for ${appsVersionNotFound.size} apps"
    )
    appsVersionNotFound.foreach(app => println(app.bundleId))

    val compatability = versionsFound.view.mapValues { version =>
      appCompatibleReason(version, device)
    }.toMap
    val compatible = compatability.filter { idCompatible =>
      idCompatible._2.isSuccess
    }
    val incompatible = compatability
      .filter { idCompatible => idCompatible._2.isFailure }
      .map { idCompatible =>
        idCompatible._1 -> idCompatible._2.failed.get.getMessage
      }
    info(
      s"there are ${compatible.size} apps on the list compatible with the device"
    )
    info(
      s"there are ${incompatible.size} apps on the list incompatible with the device"
    )
    incompatible.foreach(idReason => println(s"${idReason._1}: ${idReason._2}"))
    ActionReport(compatible.size, incompatible.size, incompatible)
  }

  def listDiff(pargs: ParsingResult, conf: Config): ActionReport = {
    val list1 = AppListParser.read(pargs.getValue[String]("list1"))
    val list2 = AppListParser.read(pargs.getValue[String]("list2"))
    val diff12 = list1.apps.diff(list2.apps)
    val diff21 = list2.apps.diff(list1.apps)
    val intersect = list1.apps.intersect(list2.apps)
    info(s"there are ${intersect.size} apps present in both lists")
    println(s"list1-list2: size ${diff12.size}")
    diff12.foreach(println)
    println(s"list2-list1: size ${diff21.size}")
    diff21.foreach(println)
    val fails = diff12.map { app =>
      app.bundleId -> app.toString
    }.toMap ++ diff21.map { app => app.bundleId -> app.toString }.toMap
    ActionReport(intersect.size, diff12.size + diff21.size, fails)
  }

}
