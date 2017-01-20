package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.ThemeManager
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.*
import javax.swing.JMenuBar

class PussycatMenuBar(width: Int, height: Int) : JMenuBar() {

    init {
        val theme = ThemeManager.currentTheme
        preferredSize = Dimension(width, height)
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = theme.getBorder(this)
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