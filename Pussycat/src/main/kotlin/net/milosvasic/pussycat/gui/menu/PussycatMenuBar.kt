package net.milosvasic.pussycat.gui.menu

import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Graphics
import javax.swing.JMenuBar


abstract class PussycatMenuBar(val theme: Theme) : JMenuBar() {

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        g.color = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        g.fillRect(0, 0, width, height)
    }

}