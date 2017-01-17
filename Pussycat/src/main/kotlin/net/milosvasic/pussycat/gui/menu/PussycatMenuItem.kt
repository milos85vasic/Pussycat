package net.milosvasic.pussycat.gui.menu

import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Graphics
import javax.swing.JMenuItem
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

class PussycatMenuItem(val theme: Theme, title: String) : JMenuItem(title) {

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(0, 0, 0, 0))
        isVisible = true
    }

}