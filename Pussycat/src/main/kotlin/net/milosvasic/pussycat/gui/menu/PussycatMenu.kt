package net.milosvasic.pussycat.gui.menu

import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Graphics
import javax.swing.JMenu


class PussycatMenu(val theme: Theme, title: String) : JMenu(title) {

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        g.color = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        g.fillRect(0, 0, width, height)
    }

}