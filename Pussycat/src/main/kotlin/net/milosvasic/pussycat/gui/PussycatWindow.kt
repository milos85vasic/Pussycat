package net.milosvasic.pussycat.gui


import net.milosvasic.pussycat.gui.menu.PussycatMainMenu
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.Theme
import java.awt.Component
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JComponent
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

    open fun close() {
        isVisible = false
        dispatchEvent(WindowEvent(this, WindowEvent.WINDOW_CLOSING))
        dispose()
    }

    protected open fun initialize() {
        iconImage = favicon
        add(mainPanel)
    }

    private fun applyTheme(comp: Component?) {
        if (comp is JComponent) {
            comp.isOpaque = true
        }
        when (comp) {
            is JPanel -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.LIGHT)
            }
            is PussycatMainMenu -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
            }
        }
    }

    override fun add(comp: Component, constraints: Any?) {
        applyTheme(comp)
        super.add(comp, constraints)
    }

    override fun add(comp: Component?): Component {
        applyTheme(comp)
        return super.add(comp)
    }

    override fun add(name: String?, comp: Component?): Component {
        applyTheme(comp)
        return super.add(name, comp)
    }

    override fun add(comp: Component?, index: Int): Component {
        applyTheme(comp)
        return super.add(comp, index)
    }

    override fun add(comp: Component?, constraints: Any?, index: Int) {
        applyTheme(comp)
        super.add(comp, constraints, index)
    }

}
