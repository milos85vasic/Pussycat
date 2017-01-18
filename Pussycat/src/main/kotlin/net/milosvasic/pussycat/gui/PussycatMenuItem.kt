package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import javax.swing.JMenuItem
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

class PussycatMenuItem(title: String) : JMenuItem(title), Themable {

    override fun apply(theme: Theme) {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(0, 0, 0, 0))
    }

}