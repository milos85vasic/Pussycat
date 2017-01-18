package net.milosvasic.pussycat.gui


import net.milosvasic.pussycat.gui.menu.PussycatMainMenuPanel
import net.milosvasic.pussycat.gui.menu.PussycatMenu
import net.milosvasic.pussycat.gui.menu.PussycatMenuBar
import net.milosvasic.pussycat.gui.menu.PussycatMenuItem
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.Theme
import java.awt.BorderLayout
import java.awt.Component
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JComponent
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

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
        applyTheme(this)
    }

    private fun applyTheme(comp: Component?) {
        if (comp is JComponent) {
            comp.isOpaque = true
        }
        when (comp) {
            is PussycatMenuBar -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.border = CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
            is PussycatMenu -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.border = CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
            is PussycatMenuItem -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.border = CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
            is PussycatMainMenuPanel -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.border = CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
            is PussycatContent -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
                comp.border = CompoundBorder(comp.border, EmptyBorder(5, 5, 5, 5))
            }
            is JFrame -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.LIGHT)
            }
            is JPanel -> {
                comp.background = theme.getColor(TYPE.BASE, INTENSITY.LIGHT)
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
