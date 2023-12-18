package de.tubs.cs.ias.applist.fdroid

import de.tubs.cs.ias.applist.AppListParser.appListFormat
import de.tubs.cs.ias.applist.fdroid.FDroidJsonProtocol.{AppMedataFormat, AppVersionsFormat, IndexFormat}
import de.tubs.cs.ias.applist.{AppListAction, MobileApp, MobileAppList}
import de.tubs.cs.ias.apps.fdroid.AppDownloader.Success
import de.tubs.cs.ias.util.FileSystemInteraction
import java.io.File
import scala.util.Try
import spray.json.{JsObject, JsValue, JsonParser, enrichAny}
import wvlet.log.LogSupport

object AppListParser extends LogSupport {

  private val baseFolder = "/home/pc/ba/thirdparty"
  private val repoName = "metatrans"
  private val internetPermission: AppPermission = AppPermission(
    "android.permission.INTERNET"
  )

  def main(args: Array[String]): Unit = {
     val folderPath = s"$baseFolder/$repoName"
     val index = readIndex(s"$folderPath/lists/index-v2.json")
//     val raw = FileSystemInteraction.readInTextFile(s"$folderPath/index-v2.json")
//     val appList = readInRepoIndex(JsonParser(raw))
//     fileSystemInteraction.writeFile(appList.toJson.prettyPrint, s"$folderPath/internet.json")
     val apps = filterApps(index, Device())
     val mobileApps = apps.map { app =>
       {
         val metadata = getAppMetadata(app._1, index)
         MobileApp(
           metadata.get.name.getOrElse(Map(("en-US", "name not present")))("en-US"),
           app._1,
           metadata.get.categories.mkString(","),
           0,
           "free",
           app._2.manifest.versionCode.toString
         )
       }
     }.toList
     val appList = MobileAppList(mobileApps, "MIXED")
     // FileSystemInteraction.writeFile(appList.toJson.prettyPrint, s"$folderPath/lists/filtered.json")
//    val deleted = FileSystemInteraction
//      .readInTextFile(s"$folderPath/wrongVersion.txt")
//      .split('\n')
//    val base = "/home/pc/ba/fdroid/20231012/apps/fdroidcl/apks"
//    deleted.foreach(line => {
//      val tokens = line.split(' ')
//      val id = tokens(0)
//      val wrongVersion = tokens(2).dropRight(1)
//
//      val path = s"$base/${id}_${wrongVersion}.apk"
//      val file = new File(path)
//      if (file.exists()) {
//        // file.delete()
//        println(s"delete $id")
//      } else {
//        println(s"$path not found")
//      }
//    })
  }

  def readIndex(path: String): Index = {
    val indexRaw = FileSystemInteraction.readInTextFile(path)
    JsonParser(indexRaw).convertTo[Index]
  }

  def getAppMetadata(appId: String, index: Index): Option[AppMetadata] = {
    for (app <- index.packages) {
      if (app._1 == appId)
        return Some(app._2.metadata)
    }
    None
  }

  def readInRepoIndex(response: JsValue): MobileAppList = {
    val packages = response match {
      case outerEnvelope: JsObject =>
        assert(
          outerEnvelope.fields.size == 2,
          s"out object should have 2 and not ${outerEnvelope.fields.size} elements"
        )
        assert(
          outerEnvelope.fields.contains("repo"),
          s"out object should hava a 'repo' field"
        )
        assert(
          outerEnvelope.fields.contains("packages"),
          s"out object should have a 'packages' field"
        )

        val packagesField = outerEnvelope.fields("packages")
        packagesField match {
          case packages: JsObject =>
            readInPackages(packages)
          case x: JsValue =>
            throw new RuntimeException(
              s"expected JsObject as repo packages field and not ${x.getClass}"
            )
        }
      case x: JsValue =>
        throw new RuntimeException(
          s"expected JsObject as outerEnvelope and not ${x.getClass}"
        )
    }
    MobileAppList(
      packages,
      AppListAction.appListCategoryName(packages)
    )
  }

  /** Parses the packages field of the F-Droid index-v2 json structure
    * into a list of MobileApp objects that are the latest version
    * of the corresponding app on F-Droid, if the latest version uses internet
    * @param packages packages field
    * @return List of MobileApp objects
    */
  private def readInPackages(packages: JsObject): List[MobileApp] = {
    packages.fields
      .map {
        case (name, appPackage: JsObject) => readInAppPackage(name, appPackage)
        case _                            => None
      }
      .toList
      .flatten
  }

  /** Parses an app pacakge from the F-Droid index-v2 json structure
    * into a MobileApp, if the latest version of the app uses the internet
    * @param appId the app id
    * @param appPackage the package field from the F-Droid index-v2 json
    * @return
    */
  private def readInAppPackage(
      appId: String,
      appPackage: JsObject
  ): Option[MobileApp] = {
    assert(
      appPackage.fields.contains("metadata"),
      s"app package should have a 'metadata' field"
    )
    assert(
      appPackage.fields.contains("versions"),
      s"app package should have a 'versions' field"
    )

    val metadataField = appPackage.fields("metadata")
    val metadata = metadataField.convertTo[AppMetadata]
    val name = metadata.name.getOrElse(Map(("en-US", "name not present")))("en-US")
    val categories = metadata.categories.getOrElse(List(""))
    val category = categories.reduce[String] {
      case (cat1: String, cat2: String) => s"$cat1,$cat2"
    }

    val appPackageVersions = appPackage.convertTo[AppVersions]
    val latestInternetVersion = latestVersionWithInternetUse(
      appPackageVersions.versions.values.toList
    )
    // val latestVersionsUsesInternet = readInAppVersions(
    //   appPackageVersions.versions
    // )
    if (latestInternetVersion.nonEmpty) {
      Some(
        MobileApp(
          name,
          appId,
          category,
          0,
          "free",
          latestInternetVersion.get.manifest.versionCode.toString
        )
      )
    } else {
      None
    }
  }

  def mapToList(
      sdk: Int,
      apps: Iterable[AppVersion]
  ): (Int, List[AppVersion]) = {
    (sdk, apps.toList)
  }

  /** Checks if an app version is compatible with an device
    * @param app the app version
    * @param device the device to run the app on
    * @return true if the app is compatible with the device
    */
  def appCompatible(app: AppVersion, device: Device): Boolean = {
    compatibleSdkVersion(app, device) && compatibleAbi(app, device)
  }

  def appCompatibleReason(app: AppVersion, device: Device): Try[Boolean] = {
    Try(if (compatibleSdkVersion(app, device)) {
      if (compatibleAbi(app, device)) {
        true
      } else {
        throw AppIncompatible(
          s"ABI not available (app: ${app.manifest.nativecode.getOrElse(List()).mkString(",")}; " +
            s"device: ${device.abis.mkString(",")})"
        )
      }
    } else {
      throw AppIncompatible(
        s"API level not supported (" +
          s"minSdk: ${app.manifest.usesSdk.get.minSdkVersion}, " +
          s"maxSdk: ${app.manifest.usesSdk.get.maxSdkVersion.getOrElse(0)}, " +
          s"device: ${device.apiLevel})"
      )
    })
  }

  private def compatibleAbi(app: AppVersion, device: Device): Boolean = {
    app.manifest.nativecode match {
      case Some(nativeCode) =>
        if (nativeCode.intersect(device.abis).isEmpty)
          return false
      case _ =>
    }
    true
  }

  private def compatibleSdkVersion(app: AppVersion, device: Device): Boolean = {
    app.manifest.usesSdk match {
      case Some(sdkUse) =>
        if (sdkUse.minSdkVersion > device.apiLevel)
          return false
        sdkUse.maxSdkVersion match {
          case Some(maxSdk) =>
            if (maxSdk > 0 && maxSdk < device.apiLevel)
              return false
          case _ =>
        }
      case _ =>
    }
    true
  }

  def getAppVersion(
      appId: String,
      versionCode: Int,
      index: Index
  ): Option[AppVersion] = {
    for (app <- index.packages) {
      if (app._1 == appId) {
        for (version <- app._2.versions) {
          if (version._2.manifest.versionCode == versionCode) {
            return Some(version._2)
          }
        }
      }
    }
    None
  }

  /** Returns the latest version of each app on the F-Droid index that fulfills all of the following conditions:
    * - the app version is compatible with the API level of the device
    * - the app version is compatible with an ABI the device supports
    * - the app version has permission to access the internet
    * @param index the F-Droid index
    * @param device the device to run the apps on
    */
  private def filterApps(
      index: Index,
      device: Device = Device()
  ): Map[String, AppVersion] = {
    println(
      s"Overall, there are ${index.packages.size} apps on the F-Droid store"
    )
    val versionAvailable = index.packages
      .filter { case (_, app) => app.versions.nonEmpty }
      .map { case (id, app) => (id, app.versions.values.toList) }
    println(
      s"There are ${versionAvailable.size} apps with at least one available version"
    )

    val compatibleSdk = filterVersions(
      versionAvailable,
      version => compatibleSdkVersion(version, device)
    )
    println(
      s"There are ${compatibleSdk.size} apps that are compatible with the API level of the device (${device.apiLevel})"
    )

    val compatibleNativeCode =
      filterVersions(compatibleSdk, version => compatibleAbi(version, device))
    println(
      s"There are ${compatibleNativeCode.size} apps that are compatible with the architecture of the device (${device
        .abis
        .mkString(",")})"
    )

    val notDeprecated = filterVersions(
      compatibleNativeCode,
      version => targetSdkNotDeprecated(version)
    )
    println(
      s"There are ${notDeprecated.size} apps that have a version that will not show a deprecated version dialog upon " +
        s"startup"
    )
    val deprecated = compatibleNativeCode.removedAll(notDeprecated.keySet)
    println(
      s"There are ${deprecated.size} apps which will show a deprecated version dialog upon startup"
    )

    val latestVersionWithInternet = compatibleNativeCode
      .map { case (id, versions) =>
        (id, latestVersionWithInternetUse(versions))
      }
      .filter { case (_, version) => version.nonEmpty }
      .map { case (id, version) => (id, version.get) }
    println(
      s"There are ${latestVersionWithInternet.size} apps that request permission to use the internet"
    )
    // val file = s"$folderPath/deprecated.txt"
    // val appIds: String = latestVersionWithInternet.values
    //   .map(v => fileNameToAppId(v.file.name))
    //   .mkString("\n")
    // FileSystemInteraction.writeFile(appIds, file)
    latestVersionWithInternet
  }

  private def filterVersions(
      apps: Map[String, List[AppVersion]],
      condition: Function[AppVersion, Boolean]
  ): Map[String, List[AppVersion]] = {
    apps
      .map { case (id, versions) =>
        (id, versions.filter(version => condition(version)))
      }
      .filter { case (_, versions) => versions.nonEmpty }
  }

  private def targetSdkNotDeprecated(app: AppVersion): Boolean = {
    app.manifest.usesSdk match {
      case Some(sdk) => sdk.targetSdkVersion >= 23
      case _         => true
    }
  }

  private def latestVersionWithInternetUse(
      versions: List[AppVersion]
  ): Option[AppVersion] = {
    val versionsLatestFirst = versions.sortBy(_.manifest.versionCode).reverse
    for (version <- versionsLatestFirst) {
      if (
        version.manifest.usesPermission.nonEmpty && version.manifest.usesPermission.get
          .contains(internetPermission)
      ) {
        return Some(version)
      }
    }
    None
  }

  private def readInAppVersions(
      versions: Map[String, AppVersion]
  ): Option[AppVersion] = {
    val latestVersionCode =
      versions.values.map[Int](v => v.manifest.versionCode).max
    val latestVersion =
      versions.find(p => p._2.manifest.versionCode == latestVersionCode).get

    latestVersion._2.manifest.usesPermission match {
      case Some(permissions) =>
        if (permissions.contains(internetPermission)) {
          Some(latestVersion._2)
        } else {
          None
        }
      case _ => None
    }
  }

  private def fileNameToAppId(fileName: String): String = {
    assert(fileName.startsWith("/"), "file names have to start with '/'")
    fileName
      .drop(1) // remove leading '/'
      .split('.')
      .dropRight(1)
      .mkString(".") // remove file ending
      .split('_')
      .dropRight(1)
      .mkString("_") // remove version code
  }

  private def groupByTargetSdk(index: Index): Map[Int, List[AppVersion]] = {
    val targetSdks = index.packages.view
      .mapValues(app =>
        app.versions.reduce((p1, p2) =>
          if (p1._2.manifest.versionCode > p2._2.manifest.versionCode) p1
          else p2
        )
      )
      .map { case (_, app) => app._2 }
      .filter(version => version.manifest.usesPermission.nonEmpty)
      .filter(version =>
        version.manifest.usesPermission.get.contains(internetPermission)
      )
      .filter(version => version.manifest.usesSdk.nonEmpty)
      .groupBy(version => version.manifest.usesSdk.get.targetSdkVersion)
      .map { case (sdk, versions) => (sdk, versions.toList) }

    targetSdks.toSeq.sortBy(_._1).foreach { case (sdk, apps) =>
      println(
        s"$sdk: size=${apps.size} ${apps.slice(0, 5).map(v => v.file.name)}"
      )
    }
    targetSdks
  }

  case class AppIncompatible(message: String) extends Exception(message: String)

  case class Device(
      abis: Array[String] = Array("x86_64", "arm64-v8a"),
      apiLevel: Int = 31
  )

}
