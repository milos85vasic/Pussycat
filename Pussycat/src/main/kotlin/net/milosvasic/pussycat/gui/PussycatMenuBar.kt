package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.*
import javax.swing.JMenuBar
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


abstract class PussycatMenuBar(val theme: Theme, width: Int, height: Int) : JMenuBar(), Themable {

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

    fun addItem(comp: Component) {
        addChild(comp)
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, 0, width, height)
        }
    }

    override fun paintBorder(g: Graphics?) {
        super.paintBorder(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, height - 1, width, 1)
        }
    }

    override fun getMinimumSize(): Dimension {
        return preferredSize
    }

}