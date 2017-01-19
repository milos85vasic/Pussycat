package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.ThemeManager
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.*
import javax.swing.JMenuBar
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


class PussycatMenuBar(width: Int, height: Int) : JMenuBar() {

    init {
        preferredSize = Dimension(width, height)
        isOpaque = true
        background = ThemeManager.currentTheme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(0, 0, 0, 0))
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