package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.ThemeManager
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import javax.swing.JPanel

class PussycatContent : JPanel() {

    init {
        val theme = ThemeManager.currentTheme
        setSize(width, 1200)
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        border = theme.getBorder(this)
    }

}