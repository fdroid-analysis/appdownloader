package de.tubs.cs.ias.apps.fdroid

import wvlet.log.LogSupport

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.sys.process._

object AppDownloader extends LogSupport {

  sealed trait FDroidResult

  object Success extends FDroidResult

  sealed trait Panic extends FDroidResult {
    val msg: String
    val context: String

    def getMessage: String = s"{$context}" + msg
  }

  case class UnknownPanic(override val msg: String,
                      override val context: String)
  extends Panic {
    override def getMessage: String ="{UNKNWON}" + super.getMessage
  }

  def readinessCheckFDroidTool(fdroidcl: String): Unit = {
    s"$fdroidcl version".!!
    // make sure the index is up-to-date
    s"$fdroidcl update".!!
  }

  def downloadApk(app: String,
                  folder: String,
                  fdroidcl: String): FDroidResult = {
    val lines: ListBuffer[String] = ListBuffer()
    try {
      val download = s"$fdroidcl download $app"
      val ret = Process(download, new File(folder), ("XDG_CACHE_HOME", s"${folder}")) ! ProcessLogger(
        _ => (),
        err => lines.append(err))
      if (ret == 0) {
        Success
      } else {
        UnknownPanic(lines.toString(), "app download")
      }
    } catch {
      case _: Throwable =>
        UnknownPanic(lines.toString(), "app download")
    }
  }


}
