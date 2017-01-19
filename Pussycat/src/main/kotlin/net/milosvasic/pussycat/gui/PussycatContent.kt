package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.ThemeManager
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import javax.swing.JPanel
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

class PussycatContent : JPanel() {

    init {
        setSize(width, 1200)
        isOpaque = true
        background = ThemeManager.currentTheme.getColor(TYPE.BASE, INTENSITY.DARK)
        border = CompoundBorder(border, EmptyBorder(5, 5, 5, 5))
    }

}