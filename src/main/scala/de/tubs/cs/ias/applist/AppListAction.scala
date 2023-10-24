package de.tubs.cs.ias.applist

import de.tubs.cs.ias.OperatingSystems.{ANDROID, FDROID, IOS, OperatingSystem}
import de.tubs.cs.ias.applist.ios.AppListDownloader.{
  download => iosAppListDownload
}
import de.tubs.cs.ias.applist.android.AppListDownloader.{
  download => androidAppListDownload
}
import de.tubs.cs.ias.applist.fdroid.AppListDownloader.{
  download => fdroidAppListDownload
}
import AppListParser.appListFormat
import de.tubs.cs.ias.util.{
  ActionReport,
  AppCategory,
  AsciiProgressBar,
  FileSystemInteraction => fsi
}
import spray.json.JsonParser
import wvlet.log.LogSupport
import java.io.File

object AppListAction extends LogSupport {

  /** Calculates the category name of an sequence of apps.
    * If all apps are in the same category, the category name.
    * If apps are in more than one category, "MIXED"
    *
    * @param apps sequence of MobileApp
    * @return the category name of the apps
    */
  def appListCategoryName(apps: Seq[MobileApp]): String = {
    val category: Set[String] =
      apps.map(_.category).map(elem => elem -> elem).toList.toMap.keySet
    if (category.size != 1) "MIXED" else category.head
  }

  /** download the chart lists for ios
    *
    * @param operatingSystem the operating system for which to download the charts
    * @param categories the categories for which to download the lists
    * @param folder the folder where to store the lists
    * @return the map of downloaded lists as well as an action report
    */
  def download(
      operatingSystem: OperatingSystem,
      categories: List[AppCategory],
      folder: String
  ): (Map[String, MobileAppList], ActionReport) = {
    assert(folder.charAt(folder.length - 1) == '/')
    val bar =
      AsciiProgressBar.create("Downloading Charts", categories.length.toLong)
    val categoryAppListMap =
      try {
        categories.map { case AppCategory(name, category) =>
          val appList = getMobileAppList(operatingSystem, category)
          val path = s"$folder/$name.json"
          AppListParser.write(appList, path)
          bar.step()

          name -> appList
        }.toMap
      } finally {
        bar.close()
      }
    (categoryAppListMap, ActionReport(categories.length, 0, Map()))
  }

  private def getMobileAppList(
      os: OperatingSystem,
      category: String
  ): MobileAppList = {
    os match {
      case IOS     => iosAppListDownload(category)
      case ANDROID => androidAppListDownload(category)
      case FDROID  => fdroidAppListDownload(category)
    }
  }

}
