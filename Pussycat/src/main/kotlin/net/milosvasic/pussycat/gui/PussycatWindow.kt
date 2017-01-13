package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.Theme
import java.awt.Component
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JFrame
import javax.swing.JPanel

abstract class PussycatWindow(val theme: Theme) : JFrame() {

    protected val mainPanel = JPanel()
    private var favicon: BufferedImage? = null

    init {
        extendedState = JFrame.MAXIMIZED_BOTH
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
    }

    open fun open() {
        initialize()
        isVisible = true
    }

    open fun close(){
        isVisible = false
        dispatchEvent(WindowEvent(this, WindowEvent.WINDOW_CLOSING))
        dispose()
    }

    override fun add(comp: Component?): Component {
        if (comp is JPanel) {
            mainPanel.isOpaque = true
            mainPanel.background = theme.getColor(TYPE.BASE, INTENSITY.LIGHT)
        }
        return super.add(comp)
    }

    protected open fun initialize() {
        iconImage = favicon
        add(mainPanel)
    }

}