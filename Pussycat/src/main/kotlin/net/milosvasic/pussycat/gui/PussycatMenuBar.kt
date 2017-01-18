package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Component
import java.awt.Container
import java.awt.Dimension
import java.awt.Graphics
import javax.swing.JMenuBar
import javax.swing.UIManager
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


open class PussycatMenuBar(val theme: Theme, width: Int, height: Int) : JMenuBar(), Themable {

    init {
        preferredSize = Dimension(width, height)
    }

    override fun apply(theme: Theme) {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(0, 0, 0, 0))
    }

    fun Container.addChild(comp: Component) {
        if (comp is Themable) {
            comp.apply(theme)
        }
        add(comp)
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, 0, width, height)
        }
    }

    override fun getMinimumSize(): Dimension {
        return preferredSize
    }

}