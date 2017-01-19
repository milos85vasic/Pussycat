package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.BorderLayout
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JFrame

abstract class PussycatWindow(val theme: Theme) : JFrame(), Themable {

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
        if (this is Themable) {
            apply(theme)
        }
    }

    override fun apply(theme: Theme?) {
        background = theme?.getColor(TYPE.BASE, INTENSITY.LIGHT)
    }

}
