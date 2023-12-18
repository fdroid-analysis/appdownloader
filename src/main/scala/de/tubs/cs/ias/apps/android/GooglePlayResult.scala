package de.tubs.cs.ias.apps.android

import scala.collection.mutable.ListBuffer

sealed trait GooglePlayResult

sealed trait Panic extends GooglePlayResult {
  val msg: String
  val context: String

  def getMessage: String = s"{$context}" + msg.replace("panic:", "")
}

case class BadRequest(override val msg: String, override val context: String)
    extends Panic

case class IncompatibleDevice(
    override val msg: String,
    override val context: String
) extends Panic

case class TooManyRequests(override val context: String) extends Panic {
  override val msg: String = "too many requests"
}

case class PurchaseRequired(override val context: String) extends Panic {
  override val msg = "purchase required"
}

case class UnknownPanic(override val msg: String, override val context: String)
    extends Panic {
  override def getMessage: String = "{UNKNOWN}" + super.getMessage
}

object Panic {

  def getPanic(panicLines: ListBuffer[String], context: String): Panic = {
    val lastLine = panicLines.filter(_.contains("panic:"))
    if (lastLine.exists(_.contains("400 Bad Request"))) {
      BadRequest(lastLine.mkString("\n"), context)
    } else if (lastLine.exists(_.contains("your device isn't compatible"))) {
      IncompatibleDevice(lastLine.mkString("\n"), context)
    } else if (lastLine.exists(_.contains("purchase required"))) {
      PurchaseRequired(context)
    } else {
      UnknownPanic(panicLines.mkString("\n"), context)
    }
  }

}

case class StringResult(value: String) extends GooglePlayResult

case class ArchitectureResult(value: Architecture) extends GooglePlayResult

object Success extends GooglePlayResult
