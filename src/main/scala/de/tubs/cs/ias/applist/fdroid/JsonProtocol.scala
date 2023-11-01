package de.tubs.cs.ias.applist.fdroid

import spray.json.{DefaultJsonProtocol, JsValue, RootJsonFormat}

object FDroidJsonProtocol extends DefaultJsonProtocol {
  implicit val AppMedataFormat: RootJsonFormat[AppMetadata] = {
    jsonFormat2(AppMetadata)
  }

  implicit val AppPermissionFormat: RootJsonFormat[AppPermission] = {
    jsonFormat1(AppPermission)
  }

  implicit val UsesSdkFormat: RootJsonFormat[UsesSdk] = {
    jsonFormat3(UsesSdk)
  }

  implicit val AppVersionManifestFormat: RootJsonFormat[AppManifest] = {
    jsonFormat5(AppManifest)
  }

  implicit val AppFileFormat: RootJsonFormat[AppFile] = {
    jsonFormat3(AppFile)
  }

  implicit val AppVersionFormat: RootJsonFormat[AppVersion] = {
    jsonFormat3(AppVersion)
  }

  implicit val AppVersionsFormat: RootJsonFormat[AppVersions] = {
    jsonFormat1(AppVersions)
  }

  implicit val AppPackageFormat: RootJsonFormat[AppPackage] = {
    jsonFormat2(AppPackage)
  }

  implicit val AntiFeatureFormat: RootJsonFormat[AntiFeature] = {
    jsonFormat2(AntiFeature)
  }

  implicit val CategoryFormat: RootJsonFormat[Category] = {
    jsonFormat1(Category)
  }

  implicit val RepoFormat: RootJsonFormat[Repo] = {
    jsonFormat6(Repo)
  }

  implicit val IndexFormat: RootJsonFormat[Index] = {
    jsonFormat2(Index)
  }

}

case class AppMetadata(name: Map[String, String],
                       categories: List[String])
case class AppPermission(name: String)

case class UsesSdk(minSdkVersion: Int,
                   targetSdkVersion: Int,
                   maxSdkVersion: Option[Int])
case class AppManifest(versionCode: Int,
                       versionName: String,
                       usesSdk: Option[UsesSdk],
                       usesPermission: Option[List[AppPermission]],
                       nativecode: Option[List[String]])

case class AppFile(name: String,
                   sha256: String,
                   size: Int)
case class AppVersion(file: AppFile,
                      manifest: AppManifest,
                      antiFeatures: Option[Map[String, JsValue]])

case class AppVersions(versions: Map[String, AppVersion])
case class AppPackage(metadata: AppMetadata,
                      versions: Map[String, AppVersion])

case class AntiFeature(description: Option[Map[String, String]],
                       name: Map[String, String])

case class Category(name: Map[String, String])
case class Repo(name: Map[String, String],
                description: Option[Map[String, String]],
                address: String,
                timestamp: Long,
                antiFeatures: Map[String, AntiFeature],
                categories: Map[String, Category])
case class Index(repo: Repo,
                 packages: Map[String, AppPackage])