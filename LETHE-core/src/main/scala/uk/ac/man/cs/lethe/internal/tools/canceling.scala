package uk.ac.man.cs.lethe.internal.tools

import com.typesafe.scalalogging.Logger

trait Cancelable {

  //private val logger = Logger(this.getClass()) ---> FAILS TO SCALA BUG: https://github.com/sbt/sbt/issues/2155
  val traitLogger = Logger[Cancelable]

  private var canceled = false

  private var dependencies = List[Cancelable]()

  def cancel =
    canceled = true

  def uncancel:Unit = {
    canceled = false
    //(dependencies.foreach(x => if(!(x==this)) x.uncancel))
  }


  def isCanceled: Boolean =
    canceled || dependencies.exists{x => if(x.isCanceled) traitLogger.info(s"${x} canceled"); x.isCanceled }

  protected def checkCanceled =
    if(isCanceled)
      throw new CanceledException()

  /**
    * Whenever this object gets canceled, other should get canceled as well.
    */
  def transferCancelInformation(other: Cancelable) = {
    other.canceled = canceled
    other.dependencies = this::other.dependencies
  }

  def detachCancelInformation(other: Cancelable) = {
    other.dependencies = other.dependencies.filterNot(_.equals(this))
  }
}

class CanceledException extends Exception {

}