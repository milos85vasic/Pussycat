package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.BorderLayout
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JFrame

abstract class PussycatWindow(val theme: Theme) : JFrame() {

    private var favicon: BufferedImage? = null

    init {
        extendedState = JFrame.MAXIMIZED_BOTH
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
    }

    open fun open() {
        initialize()
        isVisible = true
    }

    open fun close() {
        isVisible = false
        dispatchEvent(WindowEvent(this, WindowEvent.WINDOW_CLOSING))
        dispose()
    }

    protected open fun initialize() {
        iconImage = favicon
        layout = BorderLayout()
        background = theme.getColor(TYPE.BASE, INTENSITY.LIGHT)
    }


}
