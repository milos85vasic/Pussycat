package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Graphics
import java.awt.Toolkit
import javax.swing.JPopupMenu


class PussycatPopupMenu(val theme: Theme) : JPopupMenu() {

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        preferredSize.width = screenSize.width
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, 0, width, height)
        }
    }

}