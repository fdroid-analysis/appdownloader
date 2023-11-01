package de.tubs.cs.ias.apps.android

import de.tubs.cs.ias.util.AndroidConfig
import java.io.File
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.Random
import wvlet.log.LogSupport

object AppDownloader extends LogSupport {

  val DEVICE_TYPE = 1 //this is armeabi-v7a and works on our GALAXY A13
  val DELAY_TIME_MAX: Long = 10_000

  /**
    * try to call the googleplay exec
    *
    * @param googleplay path to the googleplay exec
    */
  def readinessCheckGooglePlayTool(googleplay: String): Unit =
    s"$googleplay".!!

  def getAppVersion(app: String, googleplay: String): GooglePlayResult = {
    val playCli = new GooglePlayClient(googleplay)
    playCli.getAppInfo(app) match {
      case Left(appInfo) => Result(appInfo.version.code.toString)
      case Right(panic) => panic
    }
  }

  def purchase(app: String, googleplay: String): GooglePlayResult = {
    val lines = ListBuffer[String]()
    try {
      val ret = s"$googleplay -purchase -p $DEVICE_TYPE -a $app" ! ProcessLogger(
        _ => (),
        err => lines.append(err))
      val panic = lines.filter(_.contains("panic:"))
      if (ret != 0 || panic.nonEmpty) {
        Panic.getPanic(lines, "purchase")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        Panic.getPanic(lines, "purchase")
    }
  }

  def downloadApk(app: String,
                  version: String,
                  folder: String,
                  googleplay: String): GooglePlayResult = {
    val lines: ListBuffer[String] = ListBuffer()
    try {
      Thread.sleep(Random.nextLong(DELAY_TIME_MAX)) // waiting between 0 and DELAY_TIME_MAX to avoid too many requests
      val download = s"$googleplay -p $DEVICE_TYPE -a $app -s -v $version"
      val ret = Process(download, new File(folder)) ! ProcessLogger(
        _ => (),
        err => lines.append(err))
      val panic = lines.filter(_.contains("panic:"))
      if (ret != 0) {
        Panic.getPanic(lines, "download")
      } else if (panic.nonEmpty) {
        Panic.getPanic(panic, "download")
      } else {
        Success
      }
    } catch {
      case _: Throwable =>
        val panic = lines.filter(_.contains("panic:"))
        if (panic.nonEmpty) {
          Panic.getPanic(panic, "download")
        } else {
          Panic.getPanic(lines, "download")
        }
    }
  }

  @tailrec
  def download(app: String,
               folder: String,
               config: AndroidConfig,
               maxRetries: Int = 3): GooglePlayResult = {
    getAppVersion(app, config.googleplay) match {
      case x: Panic => x
      case Success => throw new RuntimeException("getAppVersion must not return success object")
      case Result(version) =>
        purchase(app, config.googleplay) match {
          case x: Panic => x
          case Result(_) => throw new RuntimeException("purchase does not return a result")
          case Success =>
            downloadApk(app, version, folder, config.googleplay) match {
              case x: BadRequest =>
                if (maxRetries == 0) {
                  x
                } else {
                  warn("encountered bad request ... retrying")
                  Thread.sleep(Random.nextLong(DELAY_TIME_MAX))
                  download(app, folder, config, maxRetries - 1)
                }
              case panic: Panic => panic
              case Result(_) =>
                throw new RuntimeException(
                  "download Apk does not return a result")
              case Success => Success
            }

        }
    }
  }

}
