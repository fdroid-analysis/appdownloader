package de.tubs.cs.ias.applist.fdroid

import de.tubs.cs.ias.applist.fdroid.FDroidJsonProtocol.{AppMedataFormat, AppVersionsFormat, IndexFormat}
import de.tubs.cs.ias.applist.{AppListAction, MobileApp, MobileAppList}
import de.tubs.cs.ias.util.FileSystemInteraction
import spray.json.{JsObject, JsValue, JsonParser}
import wvlet.log.LogSupport
import scala.collection.MapView

object AppListParser extends LogSupport {

  private val folderPath = "/home/pc/ba/fdroid/20231012/lists/"

  private case class Device(abis: Array[String] = Array("x86_64", "arm64-v8a"),
                            apiLevel: Int = 31)

  def main(args: Array[String]): Unit = {
    val indexRaw = FileSystemInteraction.readInTextFile(s"$folderPath/index-v2.json")
    val json = JsonParser(indexRaw)
    json match {
      case jObject: JsObject =>
        filterApps(jObject.convertTo[Index], Device())
        // groupByTargetSdk(jObject.convertTo[Index])
      case _ =>
    }
  }

  def readInRepoIndex(response: JsValue): MobileAppList = {
    val packages = response match {
      case outerEnvelope: JsObject =>
        assert(
          outerEnvelope.fields.size == 2,
          s"out object should have 2 and not ${outerEnvelope.fields.size} elements")
        assert(
          outerEnvelope.fields.contains("repo"),
          s"out object should hava a 'repo' field")
        assert(
          outerEnvelope.fields.contains("packages"),
          s"out object should have a 'packages' field")

        val packagesField = outerEnvelope.fields("packages")
        packagesField match {
          case packages: JsObject =>
            readInPackages(packages)
          case x: JsValue =>
            throw new RuntimeException(
              s"expected JsObject as repo packages field and not ${x.getClass}")
        }
      case x: JsValue =>
        throw new RuntimeException(s"expected JsObject as outerEnvelope and not ${x.getClass}")
    }
    MobileAppList(
      packages,
      AppListAction.appListCategoryName(packages)
    )
  }

  private def readInPackages(packages: JsObject): List[MobileApp] = {
    packages.fields.map {
      case (name, appPackage: JsObject) => readInAppPackage(name, appPackage)
      case _ => None
    }.toList.flatten
  }

  private def readInAppPackage(bundle: String, appPackage: JsObject): Option[MobileApp] = {
    assert(
      appPackage.fields.contains("metadata"),
      s"app package should have a 'metadata' field")
    assert(
      appPackage.fields.contains("versions"),
      s"app package should have a 'versions' field")

    val metadataField = appPackage.fields("metadata")
    val metadata = metadataField.convertTo[AppMetadata]
    val name = metadata.name("en-US")
    val category = metadata.categories.reduce[String]{
      case(cat1: String, cat2: String) => s"$cat1,$cat2"}

    val appPackageVersions = appPackage.convertTo[AppVersions]
    val latestVersionsUsesInternet = readInAppVersions(appPackageVersions.versions)
    if (latestVersionsUsesInternet) {
      Option(MobileApp(
        name,
        bundle,
        category,
        0,
        "free"
      ))
    } else {
      Option.empty
    }
  }

  private val internetPermission: AppPermission = AppPermission("android.permission.INTERNET")

  private def readInAppVersions(versions: Map[String, AppVersion]): Boolean = {
    val latestVersionCode = versions.values.map[Int](v => v.manifest.versionCode).max
    val latestVersion = versions.find(p => p._2.manifest.versionCode == latestVersionCode).get

    latestVersion._2.manifest.usesPermission match {
      case Some(permissions) =>
        permissions.contains(internetPermission)
      case _ => false
    }
  }

  def mapToList(sdk: Int, apps: Iterable[AppVersion]): (Int, List[AppVersion]) = {
    (sdk, apps.toList)
  }

  private def groupByTargetSdk(index: Index): Map[Int, List[AppVersion]] = {
    val targetSdks = index.packages.view.mapValues(app =>
        app.versions.reduce((p1, p2) =>
          if (p1._2.manifest.versionCode > p2._2.manifest.versionCode) p1 else p2))
      .map {case (_, app) => app._2}
      .filter(version => version.manifest.usesPermission.nonEmpty)
      .filter(version => version.manifest.usesPermission.get.contains(internetPermission))
      .filter(version => version.manifest.usesSdk.nonEmpty)
      .groupBy(version => version.manifest.usesSdk.get.targetSdkVersion)
      .map {case (sdk, versions) => (sdk, versions.toList)}

    targetSdks.toSeq.sortBy(_._1).foreach { case (sdk, apps) =>
      println(s"$sdk: size=${apps.size} ${apps.slice(0, 5).map(v => v.file.name)}")
    }
    targetSdks
  }

  private def filterApps(index: Index, device: Device = Device()): Unit = {
    println(s"Overall, there are ${index.packages.size} apps on the F-Droid store")
    val versionAvailable = index.packages
      .filter {case (_, app) => app.versions.nonEmpty}
      .map { case (id, app) => (id, app.versions.values.toList) }
    println(s"There are ${versionAvailable.size} apps with at least one available version")

    val compatibleSdk = filterVersions(versionAvailable, version => compatibleSdkVersion(version, device))
    println(s"There are ${compatibleSdk.size} apps that are compatible with the API level of the device (${device.apiLevel})")

    val compatibleNativeCode = filterVersions(compatibleSdk, version => compatibleAbi(version, device))
    println(s"There are ${compatibleNativeCode.size} apps that are compatible with the architecture of the device (${device.abis.mkString(",")})")

    val notDeprecated = filterVersions(compatibleNativeCode, version => targetSdkNotDeprecated(version))
    println(s"There are ${notDeprecated.size} apps that have a version that will not show a deprecated version dialog upon startup")

    val deprecated = compatibleNativeCode.removedAll(notDeprecated.keySet)
    println(s"There are ${deprecated.size} apps which will show deprecated version dialog upon startup")

    val latestVersionWithInternet = deprecated
      .map { case (id, versions) => (id, latestVersionWithInternetUse(versions)) }
      .filter { case (_, version) => version.nonEmpty}
      .map { case (id, version) => (id, version.get) }
    println(s"There are ${latestVersionWithInternet.size} apps that request permission to use the internet")
    val file = s"$folderPath/deprecated.txt"
    val appIds: String = latestVersionWithInternet.values.map(v => fileNameToAppId(v.file.name)).mkString("\n")
    FileSystemInteraction.writeFile(appIds, file)
  }

  private def fileNameToAppId(fileName: String): String = {
    assert(fileName.startsWith("/"), "file names have to start with '/'")
    fileName
      .drop(1) // remove leading '/'
      .split('.').dropRight(1).mkString(".") // remove file ending
      .split('_').dropRight(1).mkString("_") // remove version code
  }

  private def filterVersions(apps: Map[String, List[AppVersion]], condition: Function[AppVersion, Boolean]): Map[String, List[AppVersion]] = {
    apps
      .map { case (id, versions) => (id, versions.filter(version => condition(version))) }
      .filter { case (_, versions) => versions.nonEmpty }
  }

  private def appCompatible(app: AppVersion, device: Device): Boolean = {
    compatibleSdkVersion(app, device) && compatibleAbi(app, device)
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

  private def targetSdkNotDeprecated(app: AppVersion): Boolean = {
    app.manifest.usesSdk match {
      case Some(sdk) => sdk.targetSdkVersion >= 23
      case _ => true
    }
  }

  private def latestVersionWithInternetUse(versions: List[AppVersion]): Option[AppVersion] = {
    val versionsLatestFirst = versions.sortBy(_.manifest.versionCode).reverse
    for (version <- versionsLatestFirst) {
      if (version.manifest.usesPermission.nonEmpty && version.manifest.usesPermission.get.contains(internetPermission)) {
        return Some(version)
      }
    }
    None
  }

}
