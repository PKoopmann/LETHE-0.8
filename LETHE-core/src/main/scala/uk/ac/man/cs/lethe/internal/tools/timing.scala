package uk.ac.man.cs.lethe.internal.tools

import java.util.Date

import scala.concurrent.TimeoutException


trait Timeoutable extends Cancelable {

  private var usesTimeout = false

  private var timeOut = 0L

  private var started = -1L

  /**
   * Sets the timeout
   * @param value the timeout in milliseconds
   */
  def useTimeout(value: Long) = {
    usesTimeout = true
    timeOut = value
  }

  def useTimeout(value: Boolean = true) =
    usesTimeout=value

  def startTiming = {
    super.uncancel
    started = new Date().getTime
  }

  def timeLeft =
    timeOut - (currentTime - started)

  def timeoutOccurred = {
    usesTimeout && currentTime - started > timeOut
  }

  def transferTimeoutInformation(other: Timeoutable) = {
    other.usesTimeout=usesTimeout
    other.timeOut = timeLeft // the timeout has to be relative, as other will start timing itself
    other.started = new Date().getTime
  }

  override def transferCancelInformation(other: Cancelable): Unit = {
    super.transferCancelInformation(other)
    other match {
      case t: Timeoutable => transferTimeoutInformation(t)
      case _ => ;
    }
  }

  protected def checkTimeout =
    if(timeoutOccurred)
      throw new TimeoutException()

  private def currentTime =
    new Date().getTime

  override def isCanceled = super.isCanceled || timeoutOccurred

  override def checkCanceled: Unit = {
    checkTimeout
    super.checkCanceled
  }
}