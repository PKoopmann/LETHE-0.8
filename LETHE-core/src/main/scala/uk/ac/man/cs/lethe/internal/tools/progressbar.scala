package uk.ac.man.cs.lethe.internal.tools


import swing.{ Label, FlowPanel, Dialog, Dimension, Button, BoxPanel, Orientation }

trait ProgressBarAttached { 
  private var _progressBar: ProgressBar = _

  def progressBar: ProgressBar = _progressBar
  def progressBar_=(newProgressBar: ProgressBar) = _progressBar = newProgressBar  

  def deactivateProgressBar = progressBar = MockProgressBar
}

trait ProgressBar { 
  var message = ""
  protected var currentValue = 0

  var maximum = 0

  def init(maximum: Int, prefix: String = "") { 
    println()
    setMaximum(maximum)
    setPrefix(prefix) 
    currentValue = 0
    redraw()
  }

  def update(newValue: Int=currentValue, newMessage: String = message): Unit = { 
    currentValue = if(newValue>maximum)
      maximum
    else
      newValue

    message = newMessage
    redraw()
  }

  def increment(newMessage: String = message): Unit = { 
    update(currentValue + 1, message)
    redraw()
  }

  def redraw()

  protected def setMaximum(int: Int) = {
    maximum = int
  }

  protected def setPrefix(prefix: String)

  def setIndeterminate(newMessage: String = message) { 
    message = newMessage
    currentValue = 0
    redraw()
  }

  def finish()
}

object MockProgressBar extends ProgressBar { 
  override def init(maximum: Int, prefix: String) = { }
  override def redraw() = { }
  override def setMaximum(max: Int) = { super.setMaximum(max) }
  override def setPrefix(prefix: String) = { }
  override def finish() = { }
}

object Tester{ 
  def main(args: Array[String]) = { 
    val progressbar = new ConsoleProgressBar()
    progressbar.init(120, "prefix: ")
    (1 to 120).foreach{ i =>
      Thread.sleep(100)
      progressbar.update(i, (i.toDouble/7).toString)
    }
  }
}

class ConsoleProgressBar extends ProgressBar { 
//  private var maximum = 0
  private var prefix = ""
  var length = 48

  private var lastStringSize=0

  override def redraw() = { 
    print("\b \b"*lastStringSize)

    val blocks = ((currentValue.toDouble/maximum)*length).toInt
    var string = prefix

    string ++= " |"
    string ++= "="*blocks
    string ++= " "*(length-blocks)//(length to(blocks, -1)).foreach(a=>string++=" ")
    string ++= "| "+ currentValue+"/"+maximum+" ("
    string ++= "%.2f" format(currentValue.toDouble/maximum*100)
    string ++= "%) "+message
    lastStringSize=string.size
    print(string)
//    (1 to 40-message.size).foreach(a=>print(" "))
//    print("\r")
  }

  def barChars = prefix.size + 2 + length + 2 + currentValue.toString.size + 1 + maximum.toString.size +
                 10 + message.size
		 

  //override def setMaximum(maximum: Int) = this.maximum=maximum
  override def setPrefix(prefix: String) = this.prefix=prefix

  override def finish() = { 
    update(maximum, "Finished!")
    println()
  }
}

class SwingProgressBar extends Dialog with ProgressBar { 

  val progressBarComponent = new swing.ProgressBar()
  val prefixLabel = new Label("  ")
  val messageLabel = new Label("  ")
  val portionLabel = new Label("0/0")
  val cancelButton = new Button("Cancel")

  progressBarComponent.preferredSize = new Dimension(400, 30)

  contents = new BoxPanel(Orientation.Vertical){ 
    contents += new FlowPanel(prefixLabel)
    contents += new FlowPanel(messageLabel)
    contents += new BoxPanel(Orientation.Horizontal){ 
//      contents += prefixLabel
      contents += progressBarComponent
      contents += portionLabel
    }
    contents += new FlowPanel(cancelButton)
  }

  centerOnScreen()
  open()

  override protected def setMaximum(int: Int) = {
    super.setMaximum(int)
    progressBarComponent.max = int
  }
  override protected def setPrefix(prefix: String) = { prefixLabel.text = prefix }

  override def redraw() = { 
    progressBarComponent.indeterminate = false
    progressBarComponent.value = currentValue
    messageLabel.text = message
    portionLabel.text = " ("+currentValue+"/"+progressBarComponent.max+")"
  }

  override def setIndeterminate(message: String) { 
    progressBarComponent.indeterminate=true 
    messageLabel.text = message
    portionLabel.text = ""
  }

  override def finish() = { }
}
