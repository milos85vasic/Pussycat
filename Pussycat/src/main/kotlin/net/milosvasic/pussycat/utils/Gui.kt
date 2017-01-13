package net.milosvasic.pussycat.utils

import java.awt.Window
import java.awt.event.WindowEvent

object Gui {

    fun show(window: Window) {
        window.isVisible = true
    }

    fun close(window: Window) {
        window.isVisible = false
        window.dispatchEvent(WindowEvent(window, WindowEvent.WINDOW_CLOSING))
    }

}