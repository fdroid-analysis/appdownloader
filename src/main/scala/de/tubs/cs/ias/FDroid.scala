package de.tubs.cs.ias

import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.OperatingSystems.FDROID
import de.tubs.cs.ias.Apmi.ensureFolderStructure
import de.tubs.cs.ias.applist.{AppListAction, MobileAppList}
import de.tubs.cs.ias.apps.AppDownloadAction
import de.tubs.cs.ias.apps.fdroid.AppDownloader
import de.tubs.cs.ias.util.ActionReportJsonWriter.actionReportCollection
import de.tubs.cs.ias.util.{ActionReport, ActionReportCollection, Config, FileSystemInteraction => fsi}
import spray.json.enrichAny
import wvlet.log.LogSupport

object FDroid extends LogSupport {


  val parser : Parser = Parser("f-droid", "perform actions for f-droid")
    .addSubparser(Parser("download", "download apps, and privacy labels")
      .addFlag("continue", "c", "continue", "if set the latest download will be resumed/restarted")
      .addSubparser(Parser("app-list", "download the current repo index only")
        .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadAppListAutomatedMain))
      .addSubparser(Parser("apps", "download a given list of apps")
        .addSubparser(Parser("manual", "manually specify download parameters")
          .addPositional("folder", "the folder into which to download")
          .addPositional("list", "a txt file containing app ids"))
        .addSubparser(Parser("automated", "leverage the config for downloading apps")
          .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadAppsAutomatedMain)))
      .addSubparser(Parser("labels", "download a given list of labels")))
    .addSubparser(Parser("full-chain", "do everything, download apps, download privacy labels")
      .addDefault[(ParsingResult, Config) => Unit]("func", this.downloadAppsMain))

  def downloadAppListAutomatedMain(pargs: ParsingResult, conf: Config): ActionReport = {
    val (_, listsFolder, _, _) = ensureFolderStructure(conf.downloadFolderRoot + conf.fdroid.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting chart list acquisition")
    val (appLists, report): (Map[String, MobileAppList], ActionReport) = AppListAction.download(FDROID, conf.fdroid.categories, listsFolder)
    appLists.foreach {
      case (category, list) =>
        info(s"category $category contains ${list.apps.length}")
    }
    info(s"overall we have ${appLists.flatMap(_._2.apps).toSet.size} unique apps")
    report
  }

  def downloadAppsAutomatedMain(pargs: ParsingResult, conf: Config) : ActionReport = {
    AppDownloader.readinessCheckFDroidTool(conf.fdroid.fdroidcl)
    val (_, _, _, appsFolder) = ensureFolderStructure(conf.downloadFolderRoot + conf.fdroid.osFolderName + "/", pargs.getValue[Boolean]("continue"))
    info("starting automated app acquisition")
    val charts = conf.fdroid.categories.map(_.name)
    val currentAppChartList = Apmi.getCurrentAppChartState(charts, conf.downloadFolderRoot + conf.fdroid.osFolderName + "/")
    val allAppIds = currentAppChartList.flatMap(_._2.apps.map(_.bundleId)).toList
    info(s"we have ${allAppIds.length} apps to download")
    val remainingApps = getRemainingApps(allAppIds, appsFolder)
    AppDownloadAction.download(
      remainingApps.slice(0, List(allAppIds.length, conf.maxAmountApps).min),
      appsFolder,
      conf,
      FDROID)
  }

  private def getRemainingApps(allApps: List[String], appsFolder: String) : List[String] = {
    val downloadFolder = s"$appsFolder/fdroidcl/apks/"

    if (!fsi.fileExists(downloadFolder)) {
      return allApps
    }

    val collectedApps = fsi.getFiles(downloadFolder, Option(".apk"))
    if (collectedApps.nonEmpty) {
      val remainingAppIds = allApps.filterNot(appId => collectedApps.exists(fileName => fileName.startsWith(appId)))
      info(s"${collectedApps.size} apps collected already: we have ${remainingAppIds.size} remaining apps to download")
      remainingAppIds
    } else {
      allApps
    }
  }

  def downloadAppsMain(pargs: ParsingResult, conf: Config) : Unit = {
    try {
      info("running full chain download")
      val (baseFolder, _, _, _) = ensureFolderStructure(conf.downloadFolderRoot + conf.fdroid.osFolderName + "/", pargs.getValue[Boolean]("continue"))
      val listReport: ActionReport = this.downloadAppListAutomatedMain(pargs, conf)
      val appsReport: ActionReport = this.downloadAppsAutomatedMain(pargs, conf)
      fsi.writeFile(ActionReportCollection(
        Map(
          "lists" -> listReport,
          "apps" -> appsReport
        )
      ).toJson.prettyPrint, s"$baseFolder/run.json")
    } catch {
      case e: Error =>
        error(e.getMessage)
      case e: Exception =>
        error(e.getMessage)
    }
  }

}
