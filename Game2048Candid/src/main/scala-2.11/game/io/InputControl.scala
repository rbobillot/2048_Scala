package game.io

import game.Grid
import org.jnativehook.{GlobalScreen, NativeHookException}
import org.jnativehook.keyboard.{NativeKeyEvent, NativeKeyListener}

import java.util.logging.{Level, LogManager, Logger}

class InputControl(grid:Grid) extends NativeKeyListener {
  override def nativeKeyTyped(e: NativeKeyEvent): Unit = ()
  override def nativeKeyPressed(e: NativeKeyEvent): Unit = ()
  override def nativeKeyReleased(e: NativeKeyEvent): Unit = e.getKeyCode match {
    case NativeKeyEvent.VC_RIGHT  => grid move "right"
    case NativeKeyEvent.VC_LEFT   => grid move "left"
    case NativeKeyEvent.VC_DOWN   => grid move "down"
    case NativeKeyEvent.VC_UP     => grid move "up"
    case NativeKeyEvent.VC_R      => grid init ()
    case NativeKeyEvent.VC_Q
      |  NativeKeyEvent.VC_ENTER
      |  NativeKeyEvent.VC_ESCAPE => stop()
    case _                        => ()
  }

  def logLevel(level:String): Unit = {
    LogManager.getLogManager.reset()
    val logger = Logger.getLogger(classOf[GlobalScreen].getPackage.getName)
    level match {
      case "WARNING" => logger.setLevel(Level.WARNING)
      case "ALL"     => logger.setLevel(Level.ALL)
      case "OFF"     => logger.setLevel(Level.OFF)
    }
    logger.setUseParentHandlers(false)
  }

  def stop():Unit = {
    GlobalScreen.unregisterNativeHook()
    sys exit 0
  }

  def init(): Unit = {
    try {
      GlobalScreen.registerNativeHook()
    } catch {
      case n: NativeHookException => println("There was a problem registering the native hook.")
      case t: Throwable           => println(t)
    }
    GlobalScreen.addNativeKeyListener(this)
  }

  logLevel("OFF")
  init()
  OutputControl.hideKeyboardOutput
}
