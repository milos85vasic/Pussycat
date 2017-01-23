package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Graphics
import javax.swing.JScrollPane


class PussycatScrollPane(val theme: Theme) : JScrollPane() {

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        border = theme.getBorder(this)
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        g?.color = background
        g?.fillRect(0, 0, width, height)
    }

    override fun paintBorder(g: Graphics?) {
        super.paintBorder(g)
        g?.color = background
        g?.fillRect(0, 0, width, height)
    }

}