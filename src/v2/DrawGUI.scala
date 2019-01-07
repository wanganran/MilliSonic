package v2

import java.awt.{BasicStroke, Color, Dimension}
import java.util

import v2.DrawGUI.LinePanel

import scala.swing.event._
import scala.swing.{Alignment, Button, Frame, Graphics2D, MainFrame, Panel, SimpleSwingApplication}

class DrawGUI extends AbstractGUI{
  val PTSIZE=0.01f
  val COLORLOW=0.05f
  val COLORHIGH=0.4f
  val YSCALE=8
  val ZSCALE=8

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

  private def getColor(dist:Float)= {
    if (dist < COLORLOW) 0f
    else if (dist > COLORHIGH) 1f
    else (dist - COLORLOW) / (COLORHIGH - COLORLOW)
  }

  def update(data: Any, draw:Boolean): Unit = {
    //print(data)
    if (panel != null)
      data match {
        case (x: Float, y: Float, z: Float) =>
          val color=getColor(x)
          panel.updateLines(Array(((y*YSCALE-PTSIZE, z*ZSCALE),(y*YSCALE+PTSIZE, z*ZSCALE)), ((y*YSCALE, z*ZSCALE-PTSIZE), (y*YSCALE, z*ZSCALE+PTSIZE))), color)
          if(draw){
            panel.addPoint((y*YSCALE,z*ZSCALE), color)
          } else panel.addPointStop()
        case (a: Float, i: Float, j: Float, k: Float) =>
      }
  }
}

object DrawGUI extends SimpleSwingApplication {
  private val gui = new DrawGUI

  //private val networking=new NetworkMain()
  //networking.start(gui)
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
    var linesColor:Float=0f

    def updateLines(lines: Array[((Float, Float), (Float, Float))], color:Float): Unit = {
      this.lines = lines
      this.linesColor=color
    }

    var points=new util.ArrayList[((Float, Float), (Float, Float), Float)]()
    var lastPoint:(Float, Float)=null
    def addPoint(point:(Float, Float), color:Float): Unit = {
      if (lastPoint != null)
        points.add((lastPoint, point, color))
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
        g.setStroke(new BasicStroke(2))
        g.setColor(Color.getHSBColor(linesColor,1f,1f))
        for (((x1, y1), (x2, y2)) <- lines) {
          g.drawLine(((x1+0.5f) * xwidth).toInt, ((y1+0.5f) * ywidth).toInt, ((x2+0.5f) * xwidth).toInt, ((y2+0.5f) * ywidth).toInt)
        }
      }

      if(points.size()>0) {
        g.setColor(Color.YELLOW)
        g.setStroke(new BasicStroke(4, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

        for (i <- 1 until points.size()) {
          val ((xp, yp), (x, y), color) = points.get(i)
          g.setColor(Color.getHSBColor(color, 1f, 1f))
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
