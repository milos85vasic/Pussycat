package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import javax.swing.JPanel
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

class PussycatContent : JPanel(), Themable {

    init {
        setSize(width, 1200)
    }

    override fun apply(theme: Theme?) {
        isOpaque = true
        background = theme?.getColor(TYPE.BASE, INTENSITY.DARK)
        border = CompoundBorder(border, EmptyBorder(5, 5, 5, 5))
    }

}