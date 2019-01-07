package v2

import java.awt.{BasicStroke, Color, Dimension}
import java.util

import config.GUIProperty
import v2.DefaultGUI.LinePanel

import scala.swing.{Alignment, Button, Frame, Graphics2D, MainFrame, Panel, SimpleSwingApplication}
import scala.swing.event._

class DefaultGUI extends AbstractGUI{
  var callback: (Symbol => Unit) = null

  def callToReset(): Unit = {
    if (callback != null)
      callback('Reset)
  }

  def callToStart(): Unit = {
    if (callback != null)
      callback('Start)
  }

  def callToStop(): Unit = {
    if (callback != null)
      callback('Stop)
  }

  def callToCalibrate(): Unit = {
    if (callback != null)
      callback('Calibrate)
  }

  private var panel: LinePanel = null

  private val translation = new Translation3DGUI(GUIProperty.XRATIO, GUIProperty.YRATIO, GUIProperty.ZRATIO)
  private var last_qua: (Float, Float, Float, Float) = (1f,0f,0f,0f)

  def update(data: Any, draw:Boolean): Unit = {
    //print(data)
    if (panel != null)
      data match {
        case (x: Float, y: Float, z: Float) =>
          if (last_qua != null) {
            panel.updateLines(translation.translateToBox((x, y, z), GUIProperty.BOX_SIZE, last_qua).map {
              case (x, y) =>
                (translation.translate(x), translation.translate(y))
            })
          }
          if(draw){
            panel.addPoint(translation.translate((x,y,z)))
          } else panel.addPointStop()
        case (a: Float, i: Float, j: Float, k: Float) => last_qua = (a, -i, -j, -k)
      }
  }
}

object DefaultGUI extends SimpleSwingApplication {
  private val gui = new DefaultGUI

  private val mainLoop=new MainLoop
  mainLoop.start(gui)
  class LinePanel extends Panel {
    _contents += new Button {
      text = "Start"
      horizontalAlignment=Alignment.Left
      reactions += {
        case ButtonClicked(_) => gui.callToStart()
      }
    }
    _contents += new Button {
      text = "Reset"
      horizontalAlignment=Alignment.Center
      reactions += {
        case ButtonClicked(_) => gui.callToReset()
      }
    }
    _contents += new Button {
      text = "Calibrate"
      horizontalAlignment=Alignment.Right
      reactions += {
        case ButtonClicked(_) => gui.callToCalibrate()
      }
    }

    _contents+=new Button{
      text="Flip Y"
      horizontalAlignment=Alignment.Left
      verticalAlignment=Alignment.Bottom
      reactions+={
        case ButtonClicked(_)=>MainLoop.ydir= -MainLoop.ydir
      }
    }
    _contents+=new Button{
      text="Flip Z"
      horizontalAlignment=Alignment.Right
      verticalAlignment=Alignment.Bottom
      reactions+={
        case ButtonClicked(_)=>MainLoop.zdir= -MainLoop.zdir
      }
    }

    var lines: Array[((Float, Float), (Float, Float))] = null

    def updateLines(lines: Array[((Float, Float), (Float, Float))]): Unit = {
      this.lines = lines
    }

    var points=new util.ArrayList[((Float, Float), (Float, Float))]()
    var lastPoint:(Float, Float)=null
    def addPoint(point:(Float, Float)): Unit = {
      if (lastPoint != null)
        points.add((lastPoint, point))
      lastPoint = point
    }

    def addPointStop():Unit={
      lastPoint=null
    }

    override def paintComponent(g: Graphics2D): Unit = {
      val xwidth = g.getClipBounds.width.toFloat
      val ywidth = g.getClipBounds.height.toFloat

      g.setBackground(Color.BLACK)
      g.clearRect(0,0,xwidth.toInt, ywidth.toInt)
      if (lines != null) {
        g.setColor(Color.WHITE)
        g.setStroke(new BasicStroke(2))
        for (((x1, y1), (x2, y2)) <- lines) {
          g.drawLine(((x1+0.5f) * xwidth).toInt, ((y1+0.5f) * ywidth).toInt, ((x2+0.5f) * xwidth).toInt, ((y2+0.5f) * ywidth).toInt)
        }
      }

      if(points.size()>0) {
        g.setColor(Color.YELLOW)
        g.setStroke(new BasicStroke(4))

        for (i <- 1 until points.size()) {
          val ((xp, yp), (x, y)) = points.get(i)
          g.drawLine(((xp + 0.5f) * xwidth).toInt, ((yp + 0.5f) * ywidth).toInt, ((x + 0.5) * xwidth).toInt, ((y + 0.5) * ywidth).toInt)
        }
      }

      this.peer.repaint()
    }
  }

  override def top: Frame = new MainFrame {
    val panel = new LinePanel
    panel.reactions += {
      case e: MousePressed => {
        gui.callToReset()
      }
    }
    gui.panel=panel
    contents = panel
    preferredSize = new Dimension(800, 800)
    size=new Dimension(800,800)
  }
}
